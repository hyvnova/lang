import threading, multiprocessing

def make_thread(daemon, target, *args, **kwargs):
    threading.Thread(target=target, args=args, kwargs=kwargs, daemon=daemon).start()

def make_process(daemon, target, *args, **kwargs):
    multiprocessing.Process(target=target, args=args, kwargs=kwargs, daemon=daemon).start()