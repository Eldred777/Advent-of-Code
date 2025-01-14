import shutil
import pathlib
import os

base_path = pathlib.Path("../")
years = ["2015", "2022", "2023"]

cwd = os.getcwd()
input_folder = os.path.join(cwd, "inputs")
# Clear contents of input_folder
if os.path.exists(input_folder):
    shutil.rmtree(input_folder)
os.makedirs(input_folder)

for year in years:
    path = os.path.join(cwd, year)
    # print(f"{path=}")
    days = os.listdir(path)
    for day in days:
        files = os.listdir(os.path.join(path, day))
        for file in files:
            new_filename = f"{year[2:]}{day}"
            if "fake" in file or "test" in file:
                new_filename += "_test"
            print(f"Moving {year} {day} {file} to {new_filename}")
            shutil.move(
                os.path.join(path, day, file),
                input_folder + "/" + new_filename,
            )

print("Done")
