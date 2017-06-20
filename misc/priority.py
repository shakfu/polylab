from datetime import date

weights = {
    'Bid'      : 1.0,
    'LeaveRequest': 0.1
}

rnd = lambda x: round(x,2)

class Task(object):
    """a simple Task class"""

    def __init__(self, kind, value, due_date=None):
        self.kind = kind
        self.value = value
        self.due_date = due_date if due_date else date.today()
        self.weight = weights[kind]
        self.urgency = 0.0
        self.importance = 0.0

    def __repr__(self):
        return '<Task: %5s - %4s - %5s - %12s - %3s - %s>' % (
            rnd(self.priority), rnd(self.importance), rnd(self.urgency),
            self.kind, self.days_to, self.value)

    def __cmp__(self, other):
        return cmp(self.priority, other.priority)

    @property
    def days_to(self):
        return (self.due_date - date.today()).days

    @property
    def priority(self):
        return self.weight * (self.importance + self.urgency)

    def relative_urgency(self, N, due_days):
        rank = due_days.index(self.days_to)
        return float(rank) / (N - 1)

    def relative_importance(self, total_value):
        return self.value / total_value

    @staticmethod
    def prioritize(tasks):
        print ("<Task: priority - importance - urgency - "
               "kind - days_to - value>\n")
        N = len(tasks)
        total_value = sum(t.value for t in tasks)
        due_days = sorted([t.days_to for t in tasks], reverse=True)
        for i in tasks:
            i.importance = i.relative_importance(total_value)
            i.urgency = i.relative_urgency(N, due_days)

        for i in sorted(tasks, reverse=True):
            print i

tasks = [
    # name               value       due_date
    Task("Bid",       1000000.0,  date(2009,4,1)),
    Task("Bid",       500400.0,   date(2009,5,1)),
    Task("Bid",       1000000.0,  date(2009,6,1)),
    Task("LeaveRequest", 0.0,        date(2009,7,1)),
    Task("LeaveRequest", 0.0,        date(2009,8,1)),
]


if __name__ == '__main__':
    Task.prioritize(tasks)
