dinamica.package("glob")

folder = dinamica.inputs["s1"]
extension = dinamica.inputs["s2"]

recurse_subfolders = dinamica.inputs["v1"] != 0

if recurse_subfolders:
    full_filename_pattern = folder + "/**/" + extension;
else:
    full_filename_pattern = folder + "/" + extension;

files = glob.glob(full_filename_pattern, recursive=recurse_subfolders)
# sort files alphabetically
files = sorted(files)
files = [["Indices*#real", "Filenames#string"]] + list(enumerate(files, 1))
# print("Prepared output for Dinamica: ", files)

dinamica.outputs["files"] = dinamica.prepareTable(files, 1)
