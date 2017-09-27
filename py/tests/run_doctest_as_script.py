def run_doctest_as_script(path):
    import doctest

    with open(path) as f:
        content = f.read()
        script = doctest.script_from_examples(content)
        exec(script)
