# GPU使用示例
# 展示如何在贝叶斯优化中使用GPU支持

import pandas as pd
from bayesian_op import run_torch_bayesian_optimization

# 使用示例

# 1. 自动选择设备（推荐）
# result = run_torch_bayesian_optimization(
#     train_data=train_df, 
#     test_data=test_df, 
#     target="your_target_column",
#     n_trials=30,
#     device="auto"  # 自动选择最佳设备
# )

# 2. 强制使用GPU
# result = run_torch_bayesian_optimization(
#     train_data=train_df, 
#     test_data=test_df, 
#     target="your_target_column",
#     n_trials=30,
#     device="cuda"  # 强制使用第一个GPU
# )

# 3. 使用特定GPU（如果有多个GPU）
# result = run_torch_bayesian_optimization(
#     train_data=train_df, 
#     test_data=test_df, 
#     target="your_target_column",
#     n_trials=30,
#     device="cuda:1"  # 使用第二个GPU
# )

# 4. 强制使用CPU
# result = run_torch_bayesian_optimization(
#     train_data=train_df, 
#     test_data=test_df, 
#     target="your_target_column",
#     n_trials=30,
#     device="cpu"  # 强制使用CPU
# )

print("GPU支持已添加到贝叶斯优化函数中！")
print("使用参数 device='auto' 让系统自动选择最佳设备")
print("使用参数 device='cuda' 强制使用GPU")
print("使用参数 device='cpu' 强制使用CPU")
