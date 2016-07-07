
root = "$HOME/Dropbox/shared/docs/content.txt"

def getcontent(path):
    with file(path) as f:
        lines = f.readlines()
    return lines


#size(800, 600)
size(1920, 1200)
# Generate compositions using random text.

#font('Arial Black')

def rndText1():
    """Returns a random string of up to 9 characters."""
    t = u""
    for i in range(random(10)):
        t += chr(random(10,120))
    return t

extras=['Was he wrong? I should like to possess a deteriorating nation.',
 'She refused to let anyone dissuade him from what he set out to do, which made him mostly horrible to everyone else.',
 'The case is not infrequently finds a very narrow, prepossessed, enchained class of spirits, who desire almost the history of society, at which society itself takes the part of this soul!',
 'Julien, who has a big face like a boar, sat down.',
 'The sudden noise behind Gregor so startled him that his father, turned around a sign to him several times.',
 'He was immediately startled, although he had to be made up for in some other way.',
 'This skepticism despises and nevertheless grasps; it undermines and takes possession; it does not like to consider vanity an exception, and is doubtful about foundation, how to leaven it with some apprehension, they would be OFF--and not back!',
 'He was preparing his case because the decisive moment was delicious but very short.',
 'One must not be some motive or touch of him!',
 'While indulging in these transports of joy which she felt a choking in his heart was troubled by his rage and unable to exhaust their admiration for his own taste.',
 'It may only be used if you charge for the use of it last night already.',
 'And let us notice what is necessary; not to be peaceable: but in a mediocre artist, one finds still oftener rough and coarse--fire and courage, and at the stake, does not thereby seek a new ill-will and counter-current... but what am I to Me?',
 "This novel interest turned him into a profound contempt for lack of dignity towards one's inferiors.",
 'I have killed--I deserve death, but that I was engaged in carrying the beds of the age.',
 'The noble type of man causes serious and unnecessary concern to his parents and fails to carry out his business duties like a boss who is hard of hearing.',
 'Nor would he have been difficult for her just to move straight ahead.']

extras2 = getcontent()

def rndText():
    xs = [ "One of the doorway, but then saw that it would be the only one condemned to work in the flat was also open he could rest."
         , "The soul of a rough coarse material."
         , "The fear of the subject and predicate conception--that is to say, in what they had to do!"
         , u"لیك چشم و گوش را آن نور نیست"
         , u"پَر و بالِ ما کمندِ عشق اوست"
         , u"مو کشانش میکشد تا کوی دوست"
         , u"我的身後，總跟著一個陰影"
         , u"من عيونِكَ ينطلقُ الضّوءُ في عُرْيِهِ الكامِل"
         , u"Either you go through this door or you don't."
         , rndText1()
         ] + extras2
    return choice(xs)

fonts = [ 'Arial'
        , 'Avenir'
        , 'Andale Mono'
        , 'Cochin'
        , 'Futura'
        ]

# Define some colors.
colormode(HSB)
white = color(1,1,1,0.8)
black = color(0,0,0,0.8)
red = color(random(),0,0.2,0.8)
rndcolor = color(random(),random(),random(),random())

translate(0,-200)
for i in range(100):
    # This translation is not reset every time, so it is
    # appended to previous translations. This gives
    # interesting effects.
    translate(random(-100,100),random(-100,100))
    # Save the current transformation. It's a good idea
    # to do this in the beginning of a loop. End the
    # loop with a pop.
    push()
    # Rotate in increments of 45 degrees.
    rotate(random(5)*45)
    fontsize(random(100))
    font(choice(fonts))
    fill(choice((white,black,red,rndcolor)))
    someText = rndText()
    text(someText, 0,0)
    pop()
