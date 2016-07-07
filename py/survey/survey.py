# survey.py
'''

QuestionType    QuestionNumber      AnswerType      AnswerNumber
================================================================
Question        1                   a               1
MultipleChoice  n                   a               1
MultiplePrefs   n                   a               n

'''


import zope.interface as zi
from zope.interface import Interface, implements, Attribute
from collections import OrderedDict



# SCHEMA ---------------------------------------------------------------

class IQuestion(Interface):
    """specifies an interface for a question type"""

    question = Attribute("question text")
    options = Attribute("[list of options: (key, option)]")
    answer = Attribute("an answer to the question")
    answer_type = Attribute("the expected type of the answer")

    def view():
        """display the question"""

    def ask():
        """ask the question"""

    def parse(response):
        """parse the answer"""

    def store():
        """store the answer"""

    def report():
        """report question and answer"""


class ITestQuestion(IQuestion):
    """interface for a test question"""

    correct_answer = Attribute("the correct answer to the question")

    def evaluate():
        """returns bool: determines whether answer is correct or otherwise"""

class IQuestionSet(Interface):
    """interface for the question set class"""

    title = Attribute("the title of the question set")
    questions = Attribute("an ordereddict of key:question")

    # properties
    answers = Attribute("propery: returns ordereddict of answers")

    def add(key, question):
        """ a -> Question -> None """

    def run():
        """run the questions in the question set"""

    def report():
        """produce a report of the question set"""


class ITestQuestionSet(IQuestionSet):
    """interface for the test question set class"""

    correct_answers = Attribute("property: returns ordereddict of answers")
    score = Attribute("percent of correct answers of total answered")


class ISurvey(Interface):
    title = Attribute("the title of the survey")
    qsets = Attribute("list of question sets")

    # properties
    answers = Attribute("all answers")

    def add(key, questionset):
        """ a -> QuestionSet -> None """

    def run():
        """run the questions in the question set"""

    def report():
        """produce a report of the question set"""




# IMPLEMENTATION -------------------------------------------------------

PROMPT = "> "


class QuestionException(Exception): pass

def colored(txt, color=None, bold=False):
    """returns text colored for the console"""
    d = dict( BOLD = "\033[1m",
              cyan = '\x1b[36m',
              pink = '\033[95m',
              blue = '\033[94m',
              green = '\033[92m',
              yellow = '\033[93m',
              red = '\033[91m',
              END = '\033[0m')
    if bold and color:
        prefix = d['BOLD'] + d[color]
    elif bold and not color:
        prefix = d['BOLD']
    elif not bold and color:
        prefix = d[color]
    else:
        raise Exception("need to specify either color string or bold bool")
    return prefix + str(txt) + d['END']

class Obdict(dict):
    def __init__(self, *args):
        self.__dict__ = self


class Question(object):
    implements(IQuestion)

    def __init__(self, question, answer_type=None):
        self.question = question
        self.answer_type = answer_type if answer_type else str
        self.options = None
        self.answer = None

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
        return self.answer_type(response)


    def store(self):
        pass

    def report(self):
        self.view()
        print self.answer


class MultipleChoice(Question):
    def __init__(self, question, options=None):
        self.question = question
        self.answer_type = type(options[0][0]) if options else str
        self.options = options
        self.answer = None

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
            self.answer_type = type(self.options[0][0])
            parsed = self.answer_type(response)
            if parsed not in dict(self.options):
                raise ValueError
            return parsed
        else:
            raise QuestionException("multiple choice question needs options")

class QuestionSet(object):
    def __init__(self, title=None, questions=None):
        ''' OrderedDict([(a, Question)]) -> Survey
        '''
        self.title = title if title else None
        self.questions = questions if questions else OrderedDict()
        self.answers = OrderedDict()

    def add(self, key, question):
        ''' a -> Question -> None
        '''
        self.questions[key] = question

    def run(self):
        print
        if self.title:
            n = len(self.title)
            print ("="*(n+2)) + '\n ' + colored(self.title, 'cyan', bold=True) + '\n' + ('='*(n+2))
        for key in self.questions:
            self.questions[key].ask()
            self.answers[key] = self.questions[key].answer

    def report(self):
        n = 0
        print
        print 'Answers:\n--------'
        for k, q in self.questions.iteritems():
            n += 1
            print "{0}. {1}".format(str(n), q.question)
            print "{0}: {1}".format(k, self.answers[k][1])
            print


def parsergen(filepath='questions.txt'):
    with file(filepath) as f:
        lines = f.readlines()
        section = []
        res = []
        title = lines.pop(0)
        lines.pop(0)
        for line in lines:
            if line.startswith('\n'):
                res.append(section)
                section = []
            else:
                section.append(line)

        print len(res)
        print "qs = QuestionSet('{0}')".format(title.strip())
        for entry in res:
            print
            options = []
            key, q = entry[0].split(':')
            q = q.strip()
            for i in entry[1:]:
                j = i.split()
                k, opt = j[0], " ".join(j[1:])
                options.append((k.strip(), opt.strip()))
            #print key, q, options
            print 'qs.add("{0}", Question("{1}", {2}))'.format(key, q, options)



class Survey:
    def __init__(self, title, qsets):
        self.title = title
        self.qsets = qsets
        self.answers = Obdict()
        self.has_run = False

    def run(self):
        for qs in self.qsets:
            qs.run()
            self.answers.update(qs.answers)
        self.has_run = True

    def report(self):
        if not self.has_run:
            self.run()
        for qs in self.qsets:
            qs.report()




#class QuestionSet: pass

#class MultipleChoice(QuestionType): pass
#class LongAnswer(QuestionType): pass
#class MultiplePreference(QuestionType): pass


def run_hay_survey():
    kh = QuestionSet('Know-How')
    kh.add('technical_know_how', MultipleChoice(
        "What is the requirement in this job for technical skills, expertise and experience?",
        [("a", "Perform simple tasks"),
         ("b", "Simple work assignments,often repetitive"),
         ("c", "Sound understanding of straightforward procedures"),
         ("d", "Practical understanding of methods, systems, processes"),
         ("e", "Conceptual understanding"),
         ("f", "In depth specialisation, broad experience"),
         ("g", "Mastery of concepts, principles : wide recognition"),
         ("h", "Externally recognized exceptional mastery")]))

    kh.add('technical_know_how_shading', MultipleChoice(
        "Please shade the technical know-how requirements for the job",
        [(1, 'less technical know-how required'),
         (2, 'avg technical know-how required'),
         (3, 'more technical know-how required'),]))

    kh.add("management_breadth", MultipleChoice(
        "What is the amount of planning and organising required of the role?",
        [(0, "no planning or organising; short timescales; not related to other tasks"),
         (1, "individual contributors who plan and organise their own work; or supervisors of subordinates whose tasks are broadly similar."),
         (2, "co-ordination and integration of services, functions or programmes pulling in broadly the same direction; one year time horizon; managing internal relationships."),
         (3, "strategic horizons; integration of diverse functions with inherent conflict"),
         (4, "total")]))

    kh.add("human_relations_skills", MultipleChoice(
        "The requirement for working with and through others to achieve the role accountabilities",
        [(1, 'Information exchange, asking questions, exercising tact'),
         (2, 'Persuasion, assertiveness - based on fact or evidence, empathy to the other\xe2\x80\x99s point of view'),
         (3, 'hearts and minds - behavior change, negotiating and partnership working')]))

    ps = QuestionSet("Problem Solving")
    ps.add("thinking_environment", MultipleChoice(
        "Assesses the extent to which thinking is constrained by the context in which it must take place",
        [('a', "Sequence and timing of action steps defined: 'do it this Example Description way'."),
         ('b', 'Selection from well-defined set of action steps based on previous experience.'),
         ('c', 'Interpretation of established precedents. Nature of problem and how to solve fairly clearly defined.'),
         ('d', 'Choose from a number of procedures in response to different work situations.'),
         ('e', 'May need to develop new procedures within existing policies'),
         ('f', 'Problem not clearly defined: Operating within broad functional guidelines.'),
         ('g', 'Developing strategy guidelines for the organisation.'),
         ('h', 'Abstractly defined')]))

    ps.add("thinking_challenge", MultipleChoice(
        "The complexity of the problems encountered and the extent to which original thinking must be employed to arrive at solutions",
        [(1, 'Stable and repetitive situations'),
         (2, 'Multiple choices based on experience; result can be readily checked for correctness'),
         (3, "Alternative solutions but a 'right answer' exists; may need analysis to identify"),
         (4, "Significant evaluative judgement; no 'right answer'; may only know if correct with hindsight"),
         (5, 'Speculative judgement; really stretching the bounds of knowledge')]))

    ps.add("thinking_challenge_shading", MultipleChoice(
        "Within the required thinking challenge parameters, please shade your prior answer",
        [(1, 'avg thinking challenged'),
         (2, 'more thinking challenged'),]))

    ac = QuestionSet("Accountability")
    ac.add("freedom_to_act", MultipleChoice(
        "How much freedom to act does the job entail?",
        [('a', 'Prescribed'),
         ('b', 'Controlled'),
         ('c', 'Standardized'),
         ('d', 'Regulated'),
         ('e', 'Directed'),
         ('f', 'Generally Directed'),
         ('g', 'Guided'),
         ('h', 'Policy-Maker guided by international best-practices')]))

    ac.add("freedom_to_act_shading", MultipleChoice(
        "Within the required freedom to act parameters, please shade your prior answer",
        [(1, 'less freedom to act'),
         (2, 'avg freedom to act'),
         (3, 'more freedom to act')]))

    ac.add("magnitude", MultipleChoice(
        "What is value of the job?",
        [(0, 'Indeterminate'),
         (1, 'Very Small'),
         (2, 'Small'),
         (3, 'Medium'),
         (4, 'Large')]))

    # TODO: should be [R,C,S,P] but lookup is not converting to ints [1,4]
    ac.add("impact", MultipleChoice(
        "What is the direct or indirect impact of the job?",
        [(1, 'Remote: lower level of contributory; distanced from area of magnitude selected'),
         (2, 'Contributory: classical "staff" slot. Advisory or service support.'),
         (3, 'Shared: partnership or joint accountability with "peer" jobs or functions inside or outside the org. Cannot share with boss or subordinate'),
         (4, 'Prime: direct and controlling impact on end results')]))

    survey = Survey("Hay Survey", [kh, ps, ac])
    survey.run()
    #survey.calculate()
    return survey

s = run_hay_survey()
