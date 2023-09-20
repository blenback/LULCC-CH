
script_lines <-readLines("Scripts/recovered_script.txt")

cleaned_lines <- sapply(script_lines, function(x) substr(x,15, nchar(x)))
write(cleaned_lines, "Scripts/cleaned_script_recover.txt")

print(cleaned_lines)
