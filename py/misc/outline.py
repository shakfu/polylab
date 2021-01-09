outline = ['1.12', '1.1', '1.4.1', '1.2']
outline.sort(key=lambda x: map(int, x.split('.')))
print outline


new_outline = ['.'.join(v) for v in (sorted([k.split('.') for k in
                                             outline]))]

