

with open(snakemake.input[0]) as f:
    content = f.read()

with open(snakemake.output[0], 'w') as f:
    f.write(content.upper())

