library(reticulate)
use_python("C:/Python312/python.exe")

run_torch_evaluation_python <- function(training_output_dir, test_data, target, device = "auto") {

  cat("Using Python for Model Evaluation...\n")
  
  # 加载Python脚本
  python_script_path <- file.path("src/s05_deep_learning/evaluate.py")
  
  if (!file.exists(python_script_path)) {
    cat("Python script not found:", python_script_path, "\n")
    return(NULL)
  }
  
  # 导入Python函数
  source_python(python_script_path)
  
  # 配置matplotlib避免内存警告
  py_run_string("
import matplotlib
matplotlib.use('Agg')  # 使用非交互式后端
import matplotlib.pyplot as plt
plt.ioff()  # 关闭交互模式
")
  
  # 转换R数据框为Python格式
  test_data_py <- r_to_py(test_data)
  
  # 调用Python函数
  result <- run_torch_evaluation(
    training_output_dir = training_output_dir,
    test_data = test_data_py,
    target = target,
    device = device
  )
  
  # 清理Python内存和图形
  py_run_string("
import matplotlib.pyplot as plt
plt.close('all')  # 关闭所有图形
import gc
gc.collect()  # 强制垃圾回收
")
  
  if (!is.null(result)) {
    cat("✓ Python Model Evaluation completed successfully\n")
    return(result)
  } else {
    cat("✗ Python Model Evaluation failed\n")
    return(NULL)
  }
}

# Load the best model and evaluate it
run_torch_evaluation_python(
  training_output_dir = training_output_dir,
  test_data = test_data,
  target = target,
  device = "auto"  # 可以改为 "cpu", "cuda" 等
)