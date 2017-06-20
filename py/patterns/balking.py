class Example(object):
    job_in_progress = False
    
    def job(self):
        if self.job_in_progress:
            return
        self.job_in_progress = True
        # put code here
        print 'execute job'

        self.job_completed()

    def job_completed(self):
        self.job_in_progress = False

if __name__ == '__main__':
    eg = Example()
    eg.job()

