library(reticulate)
use_python("C:/Python312/python.exe")

# R包装函数，使用Python的贝叶斯优化
run_torch_bayesian_optimization_python <- function(train_data, test_data, target, n_trials = 30, training_output_dir, device = "auto") {
  
  cat("Using Python for Bayesian Optimization...\n")
  
  # 加载Python脚本
  python_script_path <- file.path("src/s05_deep_learning/bayesian_op.py")

  if (!file.exists(python_script_path)) {
    cat("Python script not found:", python_script_path, "\n")
    return(NULL)
  }
  
  # 导入Python函数
  source_python(python_script_path)
  
  # 转换R数据框为Python格式
  train_data_py <- r_to_py(train_data)
  test_data_py <- r_to_py(test_data)
  
  # 调用Python函数
  result <- run_torch_bayesian_optimization(
    train_data = train_data_py,
    test_data = test_data_py,
    target = target,
    n_trials = as.integer(n_trials),
    training_output_dir = training_output_dir,
    device = device
  )
  
  # 转换结果回R格式
  if (!is.null(result)) {
    result_r <- py_to_r(result)
    
    cat("✓ Python Bayesian Optimization completed successfully\n")
    cat(sprintf("✓ Best AUC: %.4f\n", result_r$best_auc))
    
    return(result_r)
  } else {
    cat("✗ Python Bayesian Optimization failed\n")
    return(NULL)
  }
}

result <- run_torch_bayesian_optimization_python(
  train_data = train_data,
  test_data = test_data, 
  target = target,
  n_trials = 50,
  training_output_dir = training_output_dir,
  device = "cuda"  # 使用GPU
)