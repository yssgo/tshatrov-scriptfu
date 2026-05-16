#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Filename: add_viewport_meta.py
def modify_all():
    from pathlib import Path
    import re, sys
    fnquote = lambda fname: (str(fname) if re.search(r'[ \t\v\r\n]', str(
        fname)) is None else f'"{str(fname)}"')
    s_suffix = lambda i: ('s' if i != 1 else '')
    add_s = lambda word, i: word + s_suffix(i)
    i = 0
    for html in Path().glob('export-layers-plus-*.html'):
        with open(html, 'rt', encoding='utf-8') as fr:
            content = fr.read()
        viewport_meta = r'<meta name="viewport" content="width=device-width, initial-scale=1.0" />'
        matched = re.search(r'<head>([ \t]*)(\r?\n)?([ \t]*)' + viewport_meta,
                            content)
        if matched is not None:
            print(f'INFO: {fnquote(html.name)} already has vieport meta.',
                  file=sys.stderr)
            continue
        matched = re.search(r'<head>([ \t\r\n]*)', content)
        if matched is None:
            print(f'ERROR: {fnquote(html.name)} : wrong format',
                  file=sys.stderr)
            continue
        content = re.sub(r'<head>([ \t]*)(\r?\n)?([ \t]*)',
                         rf'<head>\1\2\3{viewport_meta}\2\3', content)
        with open(html, 'wt', encoding='utf-8', newline='\n') as fw:
            fw.write(content)
            print(f'INFO: {fnquote(html.name)} is modified')
            i += 1
    print(f'INFO: {i} {add_s("file", i)} modified')


if __name__ == "__main__":
    modify_all()
