# 假设你已经运行了run_torch_bayesian_optimization函数
# 并且它已经保存了'./output/best_model.pth'文件

import torch
import torch.nn.functional as F
from sklearn.base import BaseEstimator, ClassifierMixin
import numpy as np
import pandas as pd
from sklearn.metrics import roc_auc_score, accuracy_score, precision_score, recall_score, confusion_matrix, classification_report
from sklearn.inspection import PartialDependenceDisplay
import os
import matplotlib.pyplot as plt
import seaborn as sns

from model_utils import create_model, create_optimizer, train_and_evaluate

def print_cuda_memory_info(device, context=""):
    """打印CUDA内存使用信息"""
    if device.type == "cuda":
        allocated = torch.cuda.memory_allocated(device)
        reserved = torch.cuda.memory_reserved(device)
        total = torch.cuda.get_device_properties(device).total_memory
        
        print(f"🔧 CUDA Memory {context}:")
        print(f"  Allocated: {allocated / 1024**2:.1f} MB")
        print(f"  Reserved: {reserved / 1024**2:.1f} MB")
        print(f"  Total: {total / 1024**3:.1f} GB")
        print(f"  Usage: {allocated / total * 100:.1f}%")

def cleanup_cuda_memory(device):
    """清理CUDA内存"""
    if device.type == "cuda":
        torch.cuda.empty_cache()
        torch.cuda.synchronize()  # 等待所有CUDA操作完成

def load_best_model(model_path, input_dim, output_dim, device="auto"):
    param_path = os.path.join(model_path, 'bayesian_best_params.csv')
    model_state_path = os.path.join(model_path, 'best_model.pth')

    if not os.path.exists(param_path) or not os.path.exists(model_state_path):
        raise FileNotFoundError("Best model parameters or model weights not found.")
    
    # 设备检测和选择
    if device == "auto":
        if torch.cuda.is_available():
            device = torch.device("cuda")
            print(f"✓ CUDA available! Using GPU for evaluation")
        else:
            device = torch.device("cpu")
            print("✓ CUDA not available, using CPU for evaluation")
    else:
        device = torch.device(device)
        if device.type == "cuda" and not torch.cuda.is_available():
            print("⚠ CUDA requested but not available, falling back to CPU")
            device = torch.device("cpu")
    
    # 1. 加载最佳参数
    params_df = pd.read_csv(param_path)
    # 将 DataFrame 转换为字典，键为 'parameter'，值为 'value'
    best_params_raw = params_df.set_index('parameter')['value'].to_dict()

    # 构建隐藏层配置
    hidden_config = [int(best_params_raw['hidden_size_1'])]
    if best_params_raw['hidden_size_2'] > 8:
        hidden_config.append(int(best_params_raw['hidden_size_2']))
    if best_params_raw['hidden_size_3'] > 8:
        hidden_config.append(int(best_params_raw['hidden_size_3']))
    if best_params_raw['hidden_size_4'] > 8:
        hidden_config.append(int(best_params_raw['hidden_size_4']))
    # 构建 dropout 配置
    dropout_config = [best_params_raw['dropout_rate']] * len(hidden_config)
    use_batch_norm = best_params_raw['use_batch_norm_num'] > 0.5

    # 创建模型
    final_model = create_model(input_dim, hidden_config, dropout_config, output_dim, use_batch_norm, "relu")
    # 3. 加载保存的权重
    final_model.load_state_dict(torch.load(model_state_path, map_location=device))
    # 移动模型到指定设备
    final_model = final_model.to(device)

    # 4. 将模型设置为评估模式并进行推理
    final_model.eval() 

    print("✓ Best model loaded successfully.")
    print("Model Architecture:")
    print(f"  Input features: {input_dim}")
    print(f"  Hidden layers: {hidden_config}")
    print(f"  Output classes: {output_dim}")
    print(f"  Dropout rate: {best_params_raw['dropout_rate']:.4f}")
    print(f"  Batch Normalization: {'YES' if use_batch_norm else 'NO'}")
    print(f"  Device: {device}")
    
    # 显示内存使用情况
    if device.type == "cuda":
        print_cuda_memory_info(device, "after loading model")
    
    return final_model, device

def get_permutation_importance(model, X_test, y_test, feature_names, metric='auc', n_repeats=3, device=None):
    """
    计算基于排列的特征重要性。
    Args:
        model (torch.nn.Module): 训练好的模型。
        X_test (torch.Tensor): 测试集特征。
        y_test (torch.Tensor): 测试集标签。
        feature_names (list): 特征名称列表。
        metric (str): 评估指标 ('auc' 或 'accuracy')。
        n_repeats (int): 重复打乱的次数。
        device: 计算设备
    Returns:
        pd.DataFrame: 包含特征重要性的DataFrame。
    """
    if device is None:
        device = X_test.device
    
    # 清理CUDA缓存
    if device.type == "cuda":
        torch.cuda.empty_cache()
        
    model.eval()
    
    # 确保数据在正确的设备上
    X_test = X_test.to(device)
    y_test = y_test.to(device)
    
    # 获取基准分数
    with torch.no_grad():
        base_preds = F.softmax(model(X_test), dim=1).cpu().numpy()
    
    y_test_cpu = y_test.cpu().numpy()
    
    if metric == 'auc':
        if len(np.unique(y_test_cpu)) == 2:
            base_score = roc_auc_score(y_test_cpu, base_preds[:, 1])
        else:
            raise ValueError("AUC metric is only supported for binary classification.")
    elif metric == 'accuracy':
        base_score = accuracy_score(y_test_cpu, np.argmax(base_preds, axis=1))
    else:
        raise ValueError("Unsupported metric for permutation importance.")

    importance_scores = []

    for i in range(X_test.shape[1]):
        feature_scores = []
        for _ in range(n_repeats):
            # 复制数据以避免修改原始Tensor
            X_shuffled = X_test.clone()
            
            # 打乱当前特征列
            permuted_indices = torch.randperm(X_shuffled.size(0))
            X_shuffled[:, i] = X_shuffled[:, i][permuted_indices]
            
            # 重新计算分数
            with torch.no_grad():
                shuffled_preds = F.softmax(model(X_shuffled), dim=1).cpu().numpy()
            
            if metric == 'auc':
                score = roc_auc_score(y_test_cpu, shuffled_preds[:, 1])
            else: # accuracy
                score = accuracy_score(y_test_cpu, np.argmax(shuffled_preds, axis=1))
            
            feature_scores.append(base_score - score)
        
        importance_scores.append(np.mean(feature_scores))
    
    importance_df = pd.DataFrame({
        'feature': feature_names,
        'importance': importance_scores
    }).sort_values(by='importance', ascending=False)
    
    # 清理CUDA内存
    if device.type == "cuda":
        torch.cuda.empty_cache()
    
    return importance_df

def create_manual_pdp_plots(model, X_data, feature_names, device, output_dir, n_points=50):
    """
    手动创建更高质量的PDP图，完全绕过sklearn
    """
    print("Creating manual PDP plots...")
    
    # 清理现有图形和CUDA缓存
    plt.close('all')
    if device.type == "cuda":
        torch.cuda.empty_cache()
    
    n_features = len(feature_names)
    n_cols = 3
    n_rows = (n_features + n_cols - 1) // n_cols
    
    fig, axes = plt.subplots(n_rows, n_cols, figsize=(15, 4 * n_rows))
    if n_rows == 1:
        axes = axes.reshape(1, -1)
    axes = axes.flatten()
    
    model.eval()
    
    for i, feature_name in enumerate(feature_names):
        try:
            # 获取特征值范围
            feature_values = X_data[:, i]
            feature_min, feature_max = feature_values.min(), feature_values.max()
            
            # 创建特征值网格
            feature_grid = np.linspace(feature_min, feature_max, n_points)
            
            # 计算所有其他特征的均值或中位数
            base_values = np.median(X_data, axis=0)  # 使用中位数更稳健
            
            # 计算PDP值
            pdp_values = []
            
            for val in feature_grid:
                # 创建一批样本，只改变当前特征
                batch_samples = np.tile(base_values, (X_data.shape[0], 1))
                batch_samples[:, i] = val
                
                # 预测
                with torch.no_grad():
                    batch_tensor = torch.tensor(batch_samples, dtype=torch.float32).to(device)
                    probs = torch.nn.functional.softmax(model(batch_tensor), dim=1).cpu().numpy()
                    # 取正类概率的平均值
                    avg_prob = np.mean(probs[:, 1])
                    pdp_values.append(avg_prob)
            
            # 绘制PDP曲线
            axes[i].plot(feature_grid, pdp_values, linewidth=2, color='blue', marker='o', markersize=2)
            axes[i].set_title(f'PDP: {feature_name}', fontsize=12, fontweight='bold')
            axes[i].set_xlabel(feature_name, fontsize=10)
            axes[i].set_ylabel('Average Prediction', fontsize=10)
            axes[i].grid(True, alpha=0.3)
            
            # 添加特征分布的直方图作为背景
            ax2 = axes[i].twinx()
            ax2.hist(feature_values, bins=20, alpha=0.2, color='gray', density=True)
            ax2.set_ylabel('Density', fontsize=8, color='gray')
            ax2.tick_params(axis='y', labelsize=8, colors='gray')
            
        except Exception as e:
            print(f"Failed to create PDP for {feature_name}: {str(e)}")
            axes[i].text(0.5, 0.5, f'Error: {feature_name}', transform=axes[i].transAxes, 
                        ha='center', va='center', fontsize=10)
    
    # 隐藏多余的子图
    for i in range(n_features, len(axes)):
        axes[i].set_visible(False)
    
    plt.tight_layout()
    pdp_path = os.path.join(output_dir, "manual_pdp_plots.png")
    plt.savefig(pdp_path, dpi=300, bbox_inches='tight')
    plt.close()
    
    # 清理CUDA内存
    if device.type == "cuda":
        torch.cuda.empty_cache()
    
    return pdp_path

def create_ice_plots(model, X_data, feature_names, device, output_dir, n_samples=100, n_points=30):
    """
    创建Individual Conditional Expectation (ICE) 图
    """
    print("Creating ICE plots...")
    
    # 清理现有图形和CUDA缓存
    plt.close('all')
    if device.type == "cuda":
        torch.cuda.empty_cache()
    
    # 选择前6个最重要的特征
    selected_features = feature_names[:6]
    
    fig, axes = plt.subplots(2, 3, figsize=(15, 8))
    axes = axes.flatten()
    
    model.eval()
    
    # 随机选择样本子集
    n_total_samples = X_data.shape[0]
    if n_total_samples > n_samples:
        sample_indices = np.random.choice(n_total_samples, n_samples, replace=False)
        sample_data = X_data[sample_indices]
    else:
        sample_data = X_data
    
    for plot_idx, feature_name in enumerate(selected_features):
        feature_idx = feature_names.index(feature_name)
        
        try:
            # 获取特征值范围
            feature_values = X_data[:, feature_idx]
            feature_min, feature_max = feature_values.min(), feature_values.max()
            feature_grid = np.linspace(feature_min, feature_max, n_points)
            
            # 为每个样本计算ICE曲线
            ice_curves = []
            
            for sample in sample_data:
                ice_values = []
                for val in feature_grid:
                    modified_sample = sample.copy()
                    modified_sample[feature_idx] = val
                    
                    with torch.no_grad():
                        sample_tensor = torch.tensor(modified_sample.reshape(1, -1), dtype=torch.float32).to(device)
                        prob = torch.nn.functional.softmax(model(sample_tensor), dim=1).cpu().numpy()
                        ice_values.append(prob[0, 1])  # 正类概率
                
                ice_curves.append(ice_values)
                axes[plot_idx].plot(feature_grid, ice_values, alpha=0.1, color='blue', linewidth=0.5)
            
            # 计算并绘制平均PDP曲线
            pdp_curve = np.mean(ice_curves, axis=0)
            axes[plot_idx].plot(feature_grid, pdp_curve, color='red', linewidth=3, label='PDP (Average)')
            
            axes[plot_idx].set_title(f'ICE & PDP: {feature_name}', fontsize=12, fontweight='bold')
            axes[plot_idx].set_xlabel(feature_name, fontsize=10)
            axes[plot_idx].set_ylabel('Prediction', fontsize=10)
            axes[plot_idx].grid(True, alpha=0.3)
            axes[plot_idx].legend()
            
        except Exception as e:
            print(f"Failed to create ICE plot for {feature_name}: {str(e)}")
            axes[plot_idx].text(0.5, 0.5, f'Error: {feature_name}', transform=axes[plot_idx].transAxes, 
                               ha='center', va='center', fontsize=10)
    
    plt.tight_layout()
    ice_path = os.path.join(output_dir, "ice_plots.png")
    plt.savefig(ice_path, dpi=300, bbox_inches='tight')
    plt.close()
    
    # 清理CUDA内存
    if device.type == "cuda":
        torch.cuda.empty_cache()
    
    return ice_path

def create_interaction_plots(model, X_data, feature_names, device, output_dir):
    """
    创建前几个重要特征之间的交互效应图
    """
    print("Creating feature interaction plots...")
    
    # 清理现有图形和CUDA缓存
    plt.close('all')
    if device.type == "cuda":
        torch.cuda.empty_cache()
    
    # 选择前4个特征进行两两交互分析
    top_features = feature_names[:4]
    
    fig, axes = plt.subplots(2, 3, figsize=(18, 10))
    axes = axes.flatten()
    
    model.eval()
    plot_idx = 0
    
    for i in range(len(top_features)):
        for j in range(i+1, len(top_features)):
            if plot_idx >= 6:
                break
                
            feat1_name = top_features[i]
            feat2_name = top_features[j]
            feat1_idx = feature_names.index(feat1_name)
            feat2_idx = feature_names.index(feat2_name)
            
            try:
                # 获取特征范围
                feat1_values = X_data[:, feat1_idx]
                feat2_values = X_data[:, feat2_idx]
                
                feat1_grid = np.linspace(feat1_values.min(), feat1_values.max(), 20)
                feat2_grid = np.linspace(feat2_values.min(), feat2_values.max(), 20)
                
                # 创建网格
                F1, F2 = np.meshgrid(feat1_grid, feat2_grid)
                
                # 计算交互效应
                base_sample = np.median(X_data, axis=0)
                interaction_surface = np.zeros_like(F1)
                
                for ii in range(F1.shape[0]):
                    for jj in range(F1.shape[1]):
                        sample = base_sample.copy()
                        sample[feat1_idx] = F1[ii, jj]
                        sample[feat2_idx] = F2[ii, jj]
                        
                        with torch.no_grad():
                            sample_tensor = torch.tensor(sample.reshape(1, -1), dtype=torch.float32).to(device)
                            prob = torch.nn.functional.softmax(model(sample_tensor), dim=1).cpu().numpy()
                            interaction_surface[ii, jj] = prob[0, 1]
                
                # 绘制热力图
                im = axes[plot_idx].contourf(F1, F2, interaction_surface, levels=20, cmap='viridis')
                axes[plot_idx].set_title(f'{feat1_name} × {feat2_name}', fontsize=12, fontweight='bold')
                axes[plot_idx].set_xlabel(feat1_name, fontsize=10)
                axes[plot_idx].set_ylabel(feat2_name, fontsize=10)
                
                # 添加颜色条
                plt.colorbar(im, ax=axes[plot_idx], shrink=0.8)
                
            except Exception as e:
                print(f"Failed to create interaction plot for {feat1_name} × {feat2_name}: {str(e)}")
                axes[plot_idx].text(0.5, 0.5, f'Error\n{feat1_name} × {feat2_name}', 
                                   transform=axes[plot_idx].transAxes, ha='center', va='center', fontsize=10)
            
            plot_idx += 1
    
    # 隐藏多余的子图
    for idx in range(plot_idx, 6):
        axes[idx].set_visible(False)
    
    plt.tight_layout()
    interaction_path = os.path.join(output_dir, "interaction_plots.png")
    plt.savefig(interaction_path, dpi=300, bbox_inches='tight')
    plt.close()
    
    # 清理CUDA内存
    if device.type == "cuda":
        torch.cuda.empty_cache()
    
    return interaction_path

class TorchModelWrapper(BaseEstimator, ClassifierMixin):
    def __init__(self, model, device=None):
        self.model = model
        self.device = device if device is not None else next(model.parameters()).device
        self.model.eval()
        # 添加必要的sklearn属性
        self.classes_ = None
        self.n_features_in_ = None
        self.feature_names_in_ = None
        self._fitted = False
        
    def fit(self, X, y):
        """虚拟的fit方法，因为模型已经训练好了"""
        # 设置classes_属性，这是sklearn要求的
        if hasattr(y, 'numpy'):
            y_array = y.numpy() if hasattr(y, 'numpy') else y
        else:
            y_array = np.array(y)
        
        self.classes_ = np.unique(y_array)
        self.n_features_in_ = X.shape[1] if hasattr(X, 'shape') else len(X[0])
        self._fitted = True
        
        # 添加feature_names_in_如果可用
        if hasattr(X, 'columns'):
            self.feature_names_in_ = np.array(X.columns)
        elif isinstance(X, np.ndarray):
            self.feature_names_in_ = np.array([f'x{i}' for i in range(self.n_features_in_)])
            
        return self
        
    def predict(self, X):
        """预测类别"""
        import torch
        import numpy as np
        
        # 检查是否已拟合
        if not self._fitted:
            raise ValueError("This TorchModelWrapper instance is not fitted yet.")
            
        # 确保输入是numpy数组
        if not isinstance(X, np.ndarray):
            X = np.array(X)
            
        with torch.no_grad():
            X_tensor = torch.tensor(X, dtype=torch.float32).to(self.device)
            outputs = self.model(X_tensor)
            predictions = torch.argmax(outputs, dim=1).cpu().numpy()
        return predictions
        
    def predict_proba(self, X):
        """预测概率"""
        import torch
        import numpy as np
        
        # 检查是否已拟合
        if not self._fitted:
            raise ValueError("This TorchModelWrapper instance is not fitted yet.")
            
        # 确保输入是numpy数组
        if not isinstance(X, np.ndarray):
            X = np.array(X)
            
        with torch.no_grad():
            X_tensor = torch.tensor(X, dtype=torch.float32).to(self.device)
            probs = torch.nn.functional.softmax(self.model(X_tensor), dim=1).cpu().numpy()
        return probs
        
    def _check_is_fitted(self):
        """检查模型是否已拟合的内部方法"""
        return self._fitted

def run_torch_evaluation(training_output_dir, test_data, target, device="auto"):
    """加载最佳模型并在测试集上进行评估"""

    # 配置matplotlib以避免内存泄漏
    import matplotlib
    matplotlib.pyplot.ioff()  # 关闭交互模式
    
    # 清理可能存在的图形
    plt.close('all')
    
     # 数据准备
    features = [col for col in test_data.columns if col != target]
    input_dim = len(features)
    unique_labels = sorted(test_data[target].unique())
    output_dim = len(unique_labels)
    
    # 设备检测和选择
    if device == "auto":
        if torch.cuda.is_available():
            device = torch.device("cuda")
            print(f"✓ CUDA available! Using GPU for evaluation")
            # 清理初始CUDA内存
            torch.cuda.empty_cache()
            print(f"  Initial GPU Memory: {torch.cuda.memory_allocated() / 1024**2:.1f} MB")
        else:
            device = torch.device("cpu")
            print("✓ CUDA not available, using CPU for evaluation")
    else:
        device = torch.device(device)
        if device.type == "cuda" and not torch.cuda.is_available():
            print("⚠ CUDA requested but not available, falling back to CPU")
            device = torch.device("cpu")
        elif device.type == "cuda":
            torch.cuda.empty_cache()
            print(f"  Initial GPU Memory: {torch.cuda.memory_allocated() / 1024**2:.1f} MB")
    
    # 转换为torch tensors并移动到设备
    X_test = torch.tensor(test_data[features].values, dtype=torch.float32).to(device)
    # 标签映射（从0开始）
    label_mapping = {label: i for i, label in enumerate(unique_labels)}
    y_test = torch.tensor([label_mapping[label] for label in test_data[target]], dtype=torch.long).to(device)
    
    print(f"Unique labels: {unique_labels}")
    print("Label mapping (Original → Encoded):")
    for original_label, encoded_label in label_mapping.items():
        print(f"  {original_label} → {encoded_label}")
    print(f"Input features: {input_dim}, Output classes: {output_dim}")
    print(f"Test: {len(test_data)} samples")
    print(f"Device: {device}")

    final_model, model_device = load_best_model(training_output_dir, input_dim, output_dim, device)

    print("--- 正在评估模型性能 ---")
    
    # 3. 计算预测结果
    with torch.no_grad():
        predictions = final_model(X_test)
        probs = F.softmax(predictions, dim=1).cpu().numpy()
        preds_classes = np.argmax(probs, axis=1)

    y_test_np_classes = y_test.cpu().numpy()

    # 4. 计算并打印评估指标，并保存到本地
    metrics_txt = []
    if len(unique_labels) == 2:
        auc_score = roc_auc_score(y_test_np_classes, probs[:, 1])
        print(f"Test AUC Score: {auc_score:.4f}")
        metrics_txt.append(f"Test AUC Score: {auc_score:.4f}")
    else:
        auc_score = None
    accuracy = accuracy_score(y_test_np_classes, preds_classes)
    precision = precision_score(y_test_np_classes, preds_classes, average='binary')
    recall = recall_score(y_test_np_classes, preds_classes, average='binary')
    metrics_txt.append(f"Test Accuracy: {accuracy:.4f}")
    metrics_txt.append(f"Test Precision: {precision:.4f}")
    metrics_txt.append(f"Test Recall: {recall:.4f}")
    
    cm = confusion_matrix(y_test_np_classes, preds_classes)
    metrics_txt.append("\nConfusion Matrix:")
    metrics_txt.append(str(cm))
    print(f"Test Accuracy: {accuracy:.4f}")
    print(f"Test Precision: {precision:.4f}")
    print(f"Test Recall: {recall:.4f}")
    print("\nConfusion Matrix:")
    print(cm)
    
    cls_report = classification_report(y_test_np_classes, preds_classes, target_names=[str(l) for l in unique_labels])
    metrics_txt.append("\nClassification Report:")
    metrics_txt.append(cls_report)
    print("\nClassification Report:")
    print(cls_report)
    
    # 保存到本地txt文件
    metrics_path = os.path.join(training_output_dir, "metrics_and_confusion_matrix.txt")
    with open(metrics_path, "w", encoding="utf-8") as f:
        f.write("\n".join(metrics_txt))
    print(f"✓ Metrics and confusion matrix saved to {metrics_path}")
    
    # 5. 计算并保存排列特征重要性
    print("\n--- 正在计算排列特征重要性 ---")
    imp_df = get_permutation_importance(final_model, X_test, y_test, feature_names=features, metric='auc', device=device)
    print("Permutation Feature Importance:")
    print(imp_df)
    
    os.makedirs(training_output_dir, exist_ok=True)
    imp_df.to_csv(os.path.join(training_output_dir, "feature_importance.csv"), index=False)
    print(f"✓ Feature importance saved to {os.path.join(training_output_dir, 'feature_importance.csv')}")

    # 6. 可视化特征重要性
    print("--- 正在生成特征重要性图 ---")
    plt.figure(figsize=(10, 6))
    sns.barplot(x='importance', y='feature', data=imp_df)
    plt.title("Permutation Feature Importance")
    plt.xlabel("AUC Drop")
    plt.ylabel("Feature")
    plt.tight_layout()
    plt.savefig(os.path.join(training_output_dir, "feature_importance.png"), dpi=300, bbox_inches='tight')
    plt.close()  # 添加缺失的图形关闭
    
    print(f"✓ Feature importance plot saved to {os.path.join(training_output_dir, 'feature_importance.png')}")

    print("--- 正在生成多种可解释性图表 ---")
    
    # 首先尝试我们的自定义PDP图
    try:
        manual_pdp_path = create_manual_pdp_plots(
            final_model, 
            X_test.cpu().numpy(), 
            features, 
            device, 
            training_output_dir
        )
        print(f"✓ Manual PDP plots saved to {manual_pdp_path}")
    except Exception as e:
        print(f"✗ Manual PDP plots failed: {str(e)}")
    
    # 创建ICE图
    try:
        ice_path = create_ice_plots(
            final_model, 
            X_test.cpu().numpy(), 
            features, 
            device, 
            training_output_dir
        )
        print(f"✓ ICE plots saved to {ice_path}")
    except Exception as e:
        print(f"✗ ICE plots failed: {str(e)}")
    
    # 创建特征交互图
    try:
        interaction_path = create_interaction_plots(
            final_model, 
            X_test.cpu().numpy(), 
            features, 
            device, 
            training_output_dir
        )
        print(f"✓ Interaction plots saved to {interaction_path}")
    except Exception as e:
        print(f"✗ Interaction plots failed: {str(e)}")

    # 最终清理所有matplotlib图形
    plt.close('all')
    
    # 最终CUDA内存清理
    if device.type == "cuda":
        torch.cuda.empty_cache()
        final_memory = torch.cuda.memory_allocated()
        print(f"✓ Final GPU Memory: {final_memory / 1024**2:.1f} MB")
        
        # 显示总内存使用情况
        total_memory = torch.cuda.get_device_properties(device).total_memory
        print(f"✓ GPU Memory Usage: {final_memory / total_memory * 100:.1f}% of {total_memory / 1024**3:.1f} GB")
    
    return {
        'auc': auc_score if len(unique_labels) == 2 else None,
        'accuracy': accuracy,
        'precision': precision,
        'recall': recall,
        'feature_importance': imp_df
    }