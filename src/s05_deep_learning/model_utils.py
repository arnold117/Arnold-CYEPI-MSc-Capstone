# model_utils.py

import torch
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
from sklearn.metrics import roc_auc_score
import numpy as np

def create_model(input_dim, hidden_units, dropout_ratios, output_dim, use_batch_norm=False, activation="relu"):
    """创建神经网络模型"""
    layers = []
    in_features = input_dim
    
    # 选择激活函数
    if activation == "relu":
        activation_fn = nn.ReLU()
    elif activation == "leaky_relu":
        activation_fn = nn.LeakyReLU(0.01)
    elif activation == "elu":
        activation_fn = nn.ELU()
    elif activation == "gelu":
        activation_fn = nn.GELU()
    else:
        activation_fn = nn.ReLU()  # 默认
    
    if len(hidden_units) == 0:
        # 线性模型
        layers.append(nn.Linear(in_features, output_dim))
    else:
        # 添加隐藏层
        for i, hidden_size in enumerate(hidden_units):
            # 线性层
            layers.append(nn.Linear(in_features, hidden_size))
            
            # 批归一化（可选）
            if use_batch_norm:
                layers.append(nn.BatchNorm1d(hidden_size))
            
            # 激活函数
            layers.append(activation_fn)
            
            # Dropout
            if dropout_ratios[i] > 0:
                layers.append(nn.Dropout(dropout_ratios[i]))
            
            in_features = hidden_size
        
        # 输出层
        layers.append(nn.Linear(in_features, output_dim))
    
    return nn.Sequential(*layers)

def create_optimizer(model_params, optimizer_type, lr, weight_decay=0):
    """创建优化器"""
    if optimizer_type == "adam":
        return optim.Adam(model_params, lr=lr, weight_decay=weight_decay)
    elif optimizer_type == "sgd":
        return optim.SGD(model_params, lr=lr, weight_decay=weight_decay, momentum=0.9)
    elif optimizer_type == "rmsprop":
        return optim.RMSprop(model_params, lr=lr, weight_decay=weight_decay)
    elif optimizer_type == "adagrad":
        return optim.Adagrad(model_params, lr=lr, weight_decay=weight_decay)
    else:
        return optim.Adam(model_params, lr=lr, weight_decay=weight_decay)  # 默认

def train_and_evaluate(model, X_train, y_train, X_test, y_test, optimizer, loss_fn, max_epochs, patience, min_improvement, early_stop_tolerance, device=None):
    """
    训练和评估模型的通用函数。
    返回最佳AUC。
    """
    # 如果没有指定设备，从数据中推断
    if device is None:
        device = X_train.device
    
    best_auc = 0
    epochs_without_improvement = 0
    
    # 初始内存检查
    if device.type == "cuda":
        initial_memory = torch.cuda.memory_allocated()
    
    model.train()
    for epoch in range(1, max_epochs + 1):
        optimizer.zero_grad()
        outputs = model(X_train)
        loss = loss_fn(outputs, y_train)
        loss.backward()
        optimizer.step()
        
        # 定期清理计算图内存
        if epoch % 50 == 0 and device.type == "cuda":
            torch.cuda.empty_cache()
        
        if epoch % 100 == 0 or epoch == 1:
            model.eval()
            with torch.no_grad():
                predictions = model(X_test)
                probs = F.softmax(predictions, dim=1)
                
                # 将数据移动到CPU进行AUC计算
                y_test_cpu = y_test.cpu().numpy()
                probs_cpu = probs.cpu().numpy()
                
                # 假设是二分类
                if len(np.unique(y_test_cpu)) == 2:
                    current_auc = roc_auc_score(y_test_cpu, probs_cpu[:, 1])
                else:
                    current_auc = 0.5
                
                # 显示内存使用情况（可选）
                if device.type == "cuda" and epoch % 500 == 0:
                    current_memory = torch.cuda.memory_allocated()
                    print(f"    Epoch {epoch}: AUC={current_auc:.4f}, GPU Memory: {current_memory / 1024**2:.1f} MB")
            
            model.train()
            
            if current_auc > best_auc + min_improvement:
                best_auc = current_auc
                epochs_without_improvement = 0
            else:
                epochs_without_improvement += 100
            
            if epochs_without_improvement >= patience or current_auc < best_auc - early_stop_tolerance:
                break
    
    # 训练结束后清理内存
    if device.type == "cuda":
        torch.cuda.empty_cache()
        final_memory = torch.cuda.memory_allocated()
        if epoch % 500 != 0:  # 避免重复打印
            print(f"    Final: AUC={best_auc:.4f}, GPU Memory: {final_memory / 1024**2:.1f} MB")
                
    return best_auc