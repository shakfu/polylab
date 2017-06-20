# survey.py
'''

QuestionType    QuestionNumber      AnswerType      AnswerNumber
================================================================
Question        1                   a               1
MultipleChoice  n                   a               1
MultiplePrefs   n                   a               n

q_types = {
    # choice
    'choice':'Choice [radio]',
    'choice-freeform': 'Choice with a freeform option [radio]',
    'choice-multiple': 'Multiple-Choice, Multiple-Answers [checkbox]',
    'choice-multiple-freeform': 'Multiple-Choice, Multiple-Answers, plus freeform [checkbox, input]',

    # simple
    'open': 'Open Answer, single line [input]',
    'open-textfield': 'Open Answer, multi-line [textarea]',
    'choice-yesno': 'Yes/No Choice [radio]',
    'choice-yesnocomment': 'Yes/No Choice with optional comment [radio, input]',
    'choice-yesnodontknow': 'Yes/No/Don\'t know Choice [radio]',
    'comment': 'Comment Only',

    # range
    'range': 'Range of numbers [select]',

    # timeperiod
    'timeperiod': 'Time Period [input, select]',
}
'''
from collections import OrderedDict

#from survey import colored

from sqlalchemy import create_engine
from sqlalchemy import Column, Integer, String, ForeignKey
from sqlalchemy.orm import relationship, backref
from sqlalchemy.orm.collections import attribute_mapped_collection
from sqlalchemy.ext.declarative import declarative_base

#engine = create_engine('sqlite:///:memory:', echo=True)
engine = create_engine('sqlite:///db.sqlite', echo=True)
Base = declarative_base()

from sqlalchemy.orm import sessionmaker
Session = sessionmaker(bind=engine)
session = Session()


ANSWER_TYPES = {
    'str'   : str,
    'float' : float,
    'int'   : int
}


# IMPLEMENTATION ------------------------------------------------------

PROMPT = "> "




# MODEL ---------------------------------------------------------------
class User(Base):
    __tablename__ = 'users'

    id = Column(Integer, primary_key=True)
    name = Column(String)

    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return "<User('%s')>" % self.name


class Question:
    def __init__(self, title, desc, type, options=None):
        self.title = title
        self.desc = desc
        self.type = type
        self.options = options

    def __repr__(self):
        return "<Question:[%s]'%s'>" % (self.type, self.title)

class Question(Base):
    __tablename__ = 'questions'

    id = Column(Integer, primary_key=True)
    key = Column(String(50))
    question = Column(String)
    answer_type = Column(String(50))
    discriminator = Column('type', String(50))
    __mapper_args__ = {'polymorphic_on': discriminator}

    questionset_id = Column(Integer, ForeignKey('questionsets.id'))
    questionset = relationship('QuestionSet', backref=backref('questions', order_by=id))


    def __init__(self, key, question, answer_type=None):
        self.key = key
        self.question = question
        self.answer_type = answer_type if answer_type else 'str'

    def __repr__(self):
        return "<Question:'%s'>" % self.question

    def view(self):
        print
        print self.question

    def ask(self):
        self.view()
        while True:
            response = raw_input(PROMPT)
            try:
                self.answer = self.parse(response)
                break
            except ValueError:
                print colored('Answer is in wrong range or type (must be {0})'.format(
                    self.answer_type), 'red')

    def parse(self, response):
        return ANSWER_TYPES[self.answer_type](response)

    def report(self):
        self.view()
        print self.answer


class Option(Base):
    __tablename__ = 'options'

    id = Column(Integer, primary_key=True)
    key = Column(String, nullable=False)
    option = Column(String)

    question_id = Column(Integer, ForeignKey('questions.id'))
    question = relationship('Question', backref=backref('options', order_by=key))

    def __init__(self, key, option):
        self.key = key
        self.option = option

    def __repr__(self):
        return "<Option('%s':'%s')>" % (self.key, self.option)


class Answer(Base):
    __tablename__ = 'answers'

    id = Column(Integer, primary_key=True)
    answer = Column(String)
    answer_type = Column(String(50))

    user_id = Column(Integer, ForeignKey('users.id'))
    user = relationship('User', backref=backref('answers', order_by=id))

    question_id = Column(Integer, ForeignKey('questions.id'))
    question = relationship('Question', backref=backref('answers', order_by=id))

    def __init__(self, answer, answer_type=None):
        self.answer = answer
        self.answer_type = answer_type if answer_type else 'str'

    def __repr__(self):
        return "<Answer('%s' for '%s')>" % (self.answer, self.question_id)



class MultipleChoice(Question):
    __mapper_args__ = {'polymorphic_identity': 'multiplechoice'}

    def view(self):
        print
        print colored(self.question, bold=True)
        for key, option in self.options:
            print "{0}) {1}".format(
                colored(key, 'cyan', bold=True),
                option)
        print

    def parse(self, response):
        if self.options:
            parsed = ANSWER_TYPES[self.answer_type](response)
            if parsed not in dict(self.options):
                raise ValueError
            return parsed
        else:
            raise QuestionException("multiple choice question needs options")



class QuestionSet(Base):
    __tablename__ = 'questionsets'

    id = Column(Integer, primary_key=True)
    title = Column(String)
    desc = Column(String)

    def __init__(self, title=None, desc=None):
        ''' OrderedDict([(a, Question)]) -> Survey
        '''
        self.title = title
        self.desc = desc

    def __repr__(self):
        return "<QuestionSet:'%s'(%s)>" % (self.title, len(self.questions))

    def run(self):
        print
        if self.title:
            n = len(self.title)
            print ("="*(n+2)) + '\n ' + colored(self.title, 'cyan', bold=True) + '\n' + ('='*(n+2))
        for question in self.questions:
            question.ask()

    def report(self):
        n = 0
        print
        print 'Answers:\n--------'
        for k, q in self.questions.iteritems():
            n += 1
            print "{0}. {1}".format(str(n), q.question)
            print "{0}: {1}".format(k, self.answers[k][1])
            print


def test():
    Base.metadata.create_all(engine)

    kh = QuestionSet('Know-How')
    m1 = MultipleChoice("technical_know_how",
        "What is the requirement in this job for technical skills, expertise and experience?")
    m1.options = [
        Option("a", "Perform simple tasks"),
        Option("b", "Simple work assignments,often repetitive"),
        Option("c", "Sound understanding of straightforward procedures"),
        Option("d", "Practical understanding of methods, systems, processes"),
        Option("e", "Conceptual understanding"),
        Option("f", "In depth specialisation, broad experience"),
        Option("g", "Mastery of concepts, principles : wide recognition"),
        Option("h", "Externally recognized exceptional mastery")
    ]
    kh.questions.append(m1)

    session.add(kh)
    session.commit()

def qset_from_file(f):
    ''' structure of file is as follows:
    <qset_title>
    <qset_desc>

    <q_title> [<q_type>]
    <q_desc>
    <q_option_key> <q_option>

    '''
    qtypes = {
        'input' : Question,
        'choice' : MultipleChoice,
    }

    Base.metadata.create_all(engine)

    lines = file(f).readlines()

    qset = QuestionSet(
        title = lines.pop(0).strip(),
        desc = lines.pop(0).strip()
    )

    qs = []
    entry = []
    for line in lines:
        if not line == '\n':
            entry.append(line)
        else:
            qs.append(entry)
            entry = []

    qs = [q for q in qs if q]

    for q in qs:
        head, tail = q[0].split('[')
        qtitle = head.strip()
        qtype = tail.strip().rstrip(']')
        qdesc = q[1].lstrip().strip()
        print qdesc
        options = []
        for option in q[2:]:
            key, opt = option.split()[0], " ".join(option.split()[1:])
            options.append((key, opt))

        q = qtypes[qtype](qtitle, qdesc)
        for k, option in options:
            q.options.append(Option(k, option))
        qset.questions.append(q)
    session.add(qset)
    session.commit()
    return qset


qset = qset_from_file('qset.txt')
