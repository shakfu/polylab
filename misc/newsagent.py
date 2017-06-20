# newsagent.py
'''
    a multiprocessing news retriever in python 3
'''

from urllib import urlopen
from multiprocessing import Pool
from Queue import Queue, Empty
from threading import Thread
import logging

# create logger
log = logging.getLogger('newsagent')
log.setLevel(logging.DEBUG)

# Alternative formatting available on python 3.2+:
formatter = logging.Formatter(
    "%(asctime)s %(threadName)s %(levelname)s %(message)s")

# Log to stdout too
streamhandler = logging.StreamHandler()
streamhandler.setLevel(logging.DEBUG)
streamhandler.setFormatter(formatter)
log.addHandler(streamhandler)


class Agent:
    def __init__(self, url):
        self.url = url
        self.content = None
     
    def __str__(self):
        return '<{0}(url={1})>'.format(self.__class__.__name__, self.url)

    def retrieve(self):

        f = urlopen(self.url)
        self.content = f.read()#.decode('utf-8')
        log.info('retrieved %s', str(self))
        f.close()

    def run(self):
        self.retrieve()

class CodeAgent(Agent): pass
class GoogleSearchAgent(Agent): pass
class RSSAgent(Agent): pass


agent_types = {
    # target_type : agent_class
    'code' : CodeAgent,
    'search': GoogleSearchAgent,
    'rss': RSSAgent,
}

def run(agent):
    agent.run()

class Mission:
    def __init__(self, targets, **config):
        self.targets = targets
        self.n_targets = len(targets)
        self.agents = self.get_agents(targets)
        self.config = config

    def get_agents(self, targets):
        agents = []
        for t in targets:
            target_type, url = targets[t]
            agent = agent_types[target_type](url)
            agents.append(agent)
        return agents

    def start(self):
        mode = self.config.get('mode')
        if mode:
            if mode == 'multiprocess':
                log.info('mode: multiprocessing')
                pool = Pool(self.n_targets)
                pool.map(run, self.agents)
            
            elif mode == 'threads':
                log.info('mode: threads')
                def do_work(q):
                    while True:
                        try:
                            agent = q.get(block=False)
                            agent.run()
                        except Empty:
                            break

                queue = Queue()
                for agent in self.agents:
                    queue.put(agent)
                threads = [
                    Thread(target=do_work, args=(queue,)) 
                    for i in range(self.n_targets)
                ]
                for t in threads:
                    t.start()
                for t in threads:
                    t.join()
                
            else:
                raise NotImplementedError('mode not implemented')
        else:
            log.info('mode: single-thread')
            for agent in self.agents:
                agent.run()

targets = {
    # name : (target_type, url)
    'python': ('code', 
        'http://python.org'),
    
    'google': ('search', 
        'http://www.google.com'),
    
    'gnews' : ('rss', 
        'http://news.google.com/news?cf=all&ned=us&hl=en&output=rss'),
}


if __name__ == '__main__':
    for i in ['threads', 'multiprocess', None]:
        m = Mission(targets, mode=i)
        m.start()


