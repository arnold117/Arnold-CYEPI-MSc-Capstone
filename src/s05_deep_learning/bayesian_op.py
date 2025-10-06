import torch
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
import numpy as np
import pandas as pd
from sklearn.metrics import roc_auc_score
from skopt import gp_minimize
from skopt.space import Real, Integer
from skopt.utils import use_named_args
import os

from model_utils import create_model, create_optimizer, train_and_evaluate

def run_torch_bayesian_optimization(train_data, test_data, target, n_trials=30, training_output_dir="./output", device="auto"):
    """贝叶斯优化版本，包含保存最佳模型的功能"""
    
    print("Starting Torch Bayesian Optimization...")
    
    # 设备检测和选择
    if device == "auto":
        if torch.cuda.is_available():
            device = torch.device("cuda")
            print(f"✓ CUDA available! Using GPU: {torch.cuda.get_device_name()}")
            print(f"  GPU Memory: {torch.cuda.get_device_properties(0).total_memory / 1024**3:.1f} GB")
            # 清理初始GPU内存状态
            torch.cuda.empty_cache()
            print(f"  Initial GPU Memory Usage: {torch.cuda.memory_allocated() / 1024**2:.1f} MB")
        else:
            device = torch.device("cpu")
            print("✓ CUDA not available, using CPU")
    else:
        device = torch.device(device)
        if device.type == "cuda" and torch.cuda.is_available():
            print(f"✓ Using specified GPU: {torch.cuda.get_device_name()}")
            torch.cuda.empty_cache()
            print(f"  Initial GPU Memory Usage: {torch.cuda.memory_allocated() / 1024**2:.1f} MB")
        elif device.type == "cuda" and not torch.cuda.is_available():
            print("⚠ CUDA requested but not available, falling back to CPU")
            device = torch.device("cpu")
        else:
            print(f"✓ Using specified device: {device}")
    
    # 数据准备
    features = [col for col in train_data.columns if col != target]
    input_dim = len(features)
    unique_labels = sorted(train_data[target].unique())
    output_dim = len(unique_labels)
    
    # 转换为torch tensors并移动到指定设备
    X_train = torch.tensor(train_data[features].values, dtype=torch.float32).to(device)
    X_test = torch.tensor(test_data[features].values, dtype=torch.float32).to(device)
    
    # 标签映射（从0开始）并移动到设备
    label_mapping = {label: i for i, label in enumerate(unique_labels)}
    y_train = torch.tensor([label_mapping[label] for label in train_data[target]], dtype=torch.long).to(device)
    y_test = torch.tensor([label_mapping[label] for label in test_data[target]], dtype=torch.long).to(device)
    
    print(f"Unique labels: {unique_labels}")
    print("Label mapping (Original → Encoded):")
    for original_label, encoded_label in label_mapping.items():
        print(f"  {original_label} → {encoded_label}")
    print(f"Input features: {input_dim}, Output classes: {output_dim}")
    print(f"Train: {len(train_data)} samples, Test: {len(test_data)} samples")
    print(f"Device: {device}")
    
    # 数据大小检查和建议
    total_samples = len(train_data) + len(test_data)
    if device.type == "cpu" and total_samples > 10000:
        print("ℹ Large dataset detected. Consider using GPU for faster training.")
    elif device.type == "cuda" and total_samples < 1000:
        print("ℹ Small dataset detected. CPU might be sufficient for this task.")
    
    # 定义搜索空间
    dimensions = [
        Real(-4, -0.3, name='lr_log'),           # 学习率对数
        Real(-5, -1, name='l1_l2_log'),          # L1/L2正则化对数
        Real(0, 0.7, name='dropout_rate'),       # Dropout率
        Integer(0, 4, name='batch_size_idx'),    # 批处理大小索引
        Integer(0, 3, name='optimizer_idx'),     # 优化器索引
        Integer(8, 256, name='hidden_size_1'),   # 第一层
        Integer(0, 256, name='hidden_size_2'),   # 第二层（0表示不使用）
        Integer(0, 256, name='hidden_size_3'),   # 第三层（0表示不使用）
        Integer(0, 256, name='hidden_size_4'),   # 第四层（0表示不使用）
        Integer(0, 1, name='use_batch_norm_num') # 批归一化
    ]
    
    # 常量定义
    batch_sizes = [16, 32, 64, 128, 256]
    optimizer_types = ["adam", "sgd", "rmsprop", "adagrad"]
    max_epochs = 10000
    patience = 100
    min_improvement = 0.001
    early_stop_tolerance = 0.01
    
    # 用于保存最佳模型的全局变量
    global_best_auc = -1.0
    global_best_model_params = None

    @use_named_args(dimensions)
    def objective(**params):
        """目标函数（贝叶斯优化要最小化的函数，所以返回负AUC）"""
        nonlocal global_best_auc, global_best_model_params
        
        try:
            # 参数转换
            lr = 10 ** params['lr_log']
            l1_l2 = 10 ** params['l1_l2_log']
            optimizer_type = optimizer_types[params['optimizer_idx']]
            use_batch_norm = params['use_batch_norm_num'] > 0.5
            
            # 构建隐藏层配置
            hidden_config = [params['hidden_size_1']]
            if params['hidden_size_2'] > 8:
                hidden_config.append(params['hidden_size_2'])
            if params['hidden_size_3'] > 8:
                hidden_config.append(params['hidden_size_3'])  
            if params['hidden_size_4'] > 8:
                hidden_config.append(params['hidden_size_4'])
            
            # 为每层生成dropout
            dropout_config = [params['dropout_rate']] * len(hidden_config)
            
            print(f"BO Trial: lr={lr:.4f}, l1_l2={l1_l2:.4f}, layers={hidden_config}, dropout={params['dropout_rate']:.2f}, opt={optimizer_type}")
            
            # 创建和训练模型
            model = create_model(input_dim, hidden_config, dropout_config, output_dim, use_batch_norm, "relu")
            model = model.to(device)  # 移动模型到指定设备
            optimizer = create_optimizer(model.parameters(), optimizer_type, lr, l1_l2)
            loss_fn = nn.CrossEntropyLoss()

            best_auc = train_and_evaluate(
                model=model, 
                X_train=X_train, 
                y_train=y_train, 
                X_test=X_test, 
                y_test=y_test, 
                optimizer=optimizer, 
                loss_fn=loss_fn, 
                max_epochs=max_epochs, 
                patience=patience, 
                min_improvement=min_improvement, 
                early_stop_tolerance=early_stop_tolerance,
                device=device
            )

            # 检查并保存模型
            if best_auc > global_best_auc:
                global_best_auc = best_auc
                os.makedirs(training_output_dir, exist_ok=True)
                torch.save(model.state_dict(), os.path.join(training_output_dir, "best_model.pth"))
                print(f"  --> New best model saved with AUC: {global_best_auc:.4f}")
            
            # 强制内存清理
            del model, optimizer
            if device.type == "cuda":
                torch.cuda.empty_cache()
                print(f"  GPU Memory after cleanup: {torch.cuda.memory_allocated() / 1024**2:.1f} MB")

            return -best_auc
            
        except Exception as e:
            # ... (错误处理)
            return -0.5
    
    # 运行贝叶斯优化
    print(f"Starting Bayesian Optimization with {n_trials} iterations...")
    
    # 智能分配初始点和迭代数
    if n_trials <= 5:
        n_initial_points = max(1, n_trials - 1)
    else:
        n_initial_points = min(5, n_trials // 3)
    
    print(f"  Initial random points: {n_initial_points}")
    print(f"  Total evaluations: {n_trials}")
    
    # 运行优化
    result = gp_minimize(
        func=objective,
        dimensions=dimensions,
        n_calls=n_trials,
        n_initial_points=n_initial_points,
        acq_func='EI',  # Expected Improvement
        random_state=42
    )
    
    # 提取最佳结果
    best_auc = -result.fun  # 转回正值
    best_params_values = result.x
    
    # 创建参数字典
    best_params = {}
    for i, dim in enumerate(dimensions):
        best_params[dim.name] = best_params_values[i]
    
    print(f"\n=== Best Model (by Bayesian Optimization) ===")
    print(f"Best AUC: {best_auc:.4f}")
    print("Best Parameters (raw):")
    for name, value in best_params.items():
        print(f"  {name}: {value}")
    
    # 解释参数
    print("\nInterpreted Parameters:")
    print(f"  Learning Rate: {10**best_params['lr_log']:.4f}")
    print(f"  L1/L2 Regularization: {10**best_params['l1_l2_log']:.6f}")
    print(f"  Dropout Rate: {best_params['dropout_rate']:.4f}")
    print(f"  Batch Size: {batch_sizes[best_params['batch_size_idx']]}")
    print(f"  Optimizer: {optimizer_types[best_params['optimizer_idx']]}")
    print(f"  Hidden Layer 1: {best_params['hidden_size_1']} neurons")
    if best_params['hidden_size_2'] > 8:
        print(f"  Hidden Layer 2: {best_params['hidden_size_2']} neurons")
    if best_params['hidden_size_3'] > 8:
        print(f"  Hidden Layer 3: {best_params['hidden_size_3']} neurons")
    if best_params['hidden_size_4'] > 8:
        print(f"  Hidden Layer 4: {best_params['hidden_size_4']} neurons")
    print(f"  Batch Normalization: {'YES' if best_params['use_batch_norm_num'] > 0.5 else 'NO'}")
    
    # 显示网络架构
    hidden_config = [best_params['hidden_size_1']]
    if best_params['hidden_size_2'] > 8:
        hidden_config.append(best_params['hidden_size_2'])
    if best_params['hidden_size_3'] > 8:
        hidden_config.append(best_params['hidden_size_3'])
    if best_params['hidden_size_4'] > 8:
        hidden_config.append(best_params['hidden_size_4'])
    print(f"  Network Architecture: {input_dim} → {' → '.join(map(str, hidden_config))} → {output_dim}")
    
    # 保存结果
    os.makedirs(training_output_dir, exist_ok=True)
    
    # 保存优化历史
    history_data = []
    for i, (params_values, score) in enumerate(zip(result.x_iters, result.func_vals)):
        row = {'trial': i + 1, 'auc': -score}  # 转回正的AUC值
        for j, dim in enumerate(dimensions):
            row[dim.name] = params_values[j]
        history_data.append(row)
    
    history_df = pd.DataFrame(history_data)
    # 按AUC从高到低排序
    history_df = history_df.sort_values('auc', ascending=False).reset_index(drop=True)
    history_df.to_csv(os.path.join(training_output_dir, "bayesian_optimization_history.csv"), index=False)
    
    # 保存最佳参数
    best_params_data = []
    for name, value in best_params.items():
        interpreted_value = value
        if name == 'lr_log':
            interpreted_value = f"{10**value:.4f}"
        elif name == 'l1_l2_log':
            interpreted_value = f"{10**value:.6f}"
        elif name == 'batch_size_idx':
            interpreted_value = str(batch_sizes[value])
        elif name == 'optimizer_idx':
            interpreted_value = optimizer_types[value]
        elif name == 'use_batch_norm_num':
            interpreted_value = "TRUE" if value > 0.5 else "FALSE"
        else:
            interpreted_value = f"{value:.0f}" if 'hidden_size' in name else f"{value:.4f}"
            
        best_params_data.append({
            'parameter': name,
            'value': value,
            'interpreted_value': str(interpreted_value)
        })
    
    best_params_df = pd.DataFrame(best_params_data)
    best_params_df.to_csv(os.path.join(training_output_dir, "bayesian_best_params.csv"), index=False)
    
    print("✓ Files saved:")
    print("  - bayesian_optimization_history.csv")
    print("  - bayesian_best_params.csv")
    print("  - best_model.pth (Best model's state_dict)")
    
    return {
        'best_params': best_params,
        'best_auc': best_auc,
        'history': history_df
    }