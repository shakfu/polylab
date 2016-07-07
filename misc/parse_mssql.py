
lines = file('npc.sql').readlines()
# print len(lines)

ts = []
for i, line in enumerate(lines):
    if line.startswith("CREATE TABLE"):
        # print i, line.strip()
        t = [i]
    if line.startswith(") ON [PRIMARY]"):
        # print i, line.strip()
        t.append(i)
        ts.append(t)

# for i in ts:
#    print i


tables = []

for t in ts:
    table = lines[t[0]:t[1]]
    tables.append(table)

# print len(tables)

clean = lambda s: s.replace(
    '[', '').replace(']', '').rstrip(')').rstrip('(').lower().strip()

types = set()

mappings = {
    "nchar": "char",
    "nvarchar": "char",
    "varchar": "char",
    "int": "int",
    "numeric": "decimal",
    "float": "decimal",
    "decimal": "decimal",
    "date": "date",
}


def underscore_to_camelcase(value):
    def camelcase():
        yield str.lower
        while True:
            yield str.capitalize

    c = camelcase()
    res = "".join(c.next()(x) if x else '_' for x in value.split("_"))
    return res[0].upper() + res[1:]


for table in tables:
    print
    for line in table:
        if line.startswith('CREATE'):
            # declare a table
            name = clean(
                line.replace('CREATE TABLE [dbo].[', '')).replace('(', '')
            # print name
            print "class {0}(models.Model):".format(
                underscore_to_camelcase(name))

        else:
            # print '\t', clean(line)
            txt = clean(line).split()
            try:
                types.add(txt[1])
                # print 'field:', txt[0]
                for key in mappings.keys():
                    if txt[1].startswith(key):
                        #~ print 'type', mappings[key]
                        print "\t{0} = {1}()".format(txt[0], mappings[key])
            # print txt
            except IndexError, e:
                # print txt
                continue

# print types
