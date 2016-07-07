# import jinja2

# env = jinja2.Environment(
#     extensions=['jinja2.ext.loopcontrols'],
#     keep_trailing_newline=True,
#     # autoescape=False,
#     trim_blocks=True,
# )
# template = '''
# Jinja Template:
# {% for item in items %}
#     {{item}}
# {% endfor %}
# '''

# t = env.from_string(template)
# print t.render(items=['a', 'b', 'c'])


from mako.template import Template

t = Template("""
Mako Template:
%for item in items:
    ${item}
%endfor 
""")

print t.render(items=['a', 'b', 'c'])


