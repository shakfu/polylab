import os
#from sap.mixins import Tool
from sap.configuration import Config
from sap.log import Logger
from sap.services import translate


class Localizer(object):
    languages = ['en', 'ar', 'es']

    def __init__(self, options=None):
        self.c = Config().db
        self.options = options
        self.log = Logger(self.__class__.__name__, options)
        self.pot = "{po}/{appname}.pot".format(
            po=self.c.dirs.po,
            appname=self.c.appname)

    def po(self, lang):
        return "{po}/{lang}.po".format(
            po=self.c.dirs.po, lang=lang)

    def process(self):
        self.make_pot()
        self.modify_pot()
        self.make_po()
        self.translate_po()
        self.make_mo()

    def cmd(self, txt):
        self.log.info(txt)
        os.system(txt)

    def make_pot(self):
        self.cmd(('xgettext --language=Python --keyword=_ '
                  '--output={po}/{appname}.pot --from-code=UTF-8'
                  ' `find . -name "*.py"`').format(
            po=self.c.dirs.po, appname=self.c.appname))

    def modify_pot(self):
        self.log.info('convert charset -> utf-8 in template')
        with file(self.pot) as f:
            pot = f.read()
            pot = pot.replace('CHARSET', 'UTF-8')
        with file(self.pot, 'w') as f:
            f.write(pot)

    def make_po(self):
        for lang in self.languages:
            self.cmd((
                'msginit --input={po}/{appname}.pot '
                '--locale={lang} --output={po}/{lang}.po').format(
                po=self.c.dirs.po, lang=lang, appname=self.c.appname))

    def translate_po(self):
        for lang in self.languages:
            if lang == 'en':
                continue
            filename = self.po(lang)
            self.log.info('{} processed', filename)
            with file(filename) as f:
                lines = f.readlines()
                header, rawlines = lines[:18], lines[18:]
                msgs = []
                for line in rawlines:
                    if line.startswith('msgid'):
                        msgs.append(line[7:].strip()[:-1])
                translated = zip(msgs,
                                 translate(msgs,
                                           from_lang='en',
                                           to_lang=lang))
                self.log.info('translated: {}', translated)
                for key, value in translated:
                    header.append('msgid "{}"\n'.format(key))
                    header.append(u'msgstr "{}"\n'.format(value))
                    header.append('\n')
                #print "".join(header)
            with file(filename, 'w') as f:
                f.write("".join(header).encode('utf-8'))
                self.log.info('translated: {}', filename)

    def make_mo(self):
        for lang in self.languages:
            self.cmd(('msgfmt {po}/{lang}.po --output-file='
                      '{locale}/{lang}/LC_MESSAGES/{appname}.mo').format(
                po=self.c.dirs.po,
                lang=lang,
                locale=self.c.dirs.locale,
                appname=self.c.appname))
