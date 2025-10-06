import os
import shutil

# 获取当前脚本所在目录
target_dir = os.path.dirname(os.path.abspath(__file__))

# 遍历所有子文件夹
for root, dirs, files in os.walk(target_dir, topdown=False):
    for d in dirs:
        folder_path = os.path.join(root, d)
        try:
            # 只统计文件（不含子文件夹）
            file_list = [f for f in os.listdir(folder_path) if os.path.isfile(os.path.join(folder_path, f))]
            if len(file_list) == 1:
                print(f"Deleting folder: {folder_path} (contains only: {file_list[0]})")
                shutil.rmtree(folder_path)
        except Exception as e:
            print(f"Error processing {folder_path}: {e}")
