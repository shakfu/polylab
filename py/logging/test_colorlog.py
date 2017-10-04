#!/usr/bin/env python
"""python-colorlog example."""

import logging
import logging.config
# from colorlog import ColoredFormatter


# def setup_logger():
#     """Return a logger with a default ColoredFormatter."""
#     formatter = ColoredFormatter(
#         "%(log_color)s%(levelname)-8s%(reset)s %(blue)s%(message)s",
#         datefmt=None,
#         reset=True,
#         log_colors={
#             'DEBUG':    'cyan',
#             'INFO':     'green',
#             'WARNING':  'yellow',
#             'ERROR':    'red',
#             'CRITICAL': 'red',
#         }
#     )
#
#     logger = logging.getLogger('example')
#     handler = logging.StreamHandler()
#     handler.setFormatter(formatter)
#     logger.addHandler(handler)
#     logger.setLevel(logging.DEBUG)
#
#     return logger
#
#
def test_logger(logger):
    logger.debug('a debug message')
    logger.info('an info message')
    logger.warning('a warning message')
    logger.error('an error message')
    logger.critical('a critical message')
#
# def test_normal():
#     """Create and use a logger."""
#     logger = setup_logger()
#     test_logger(logger)

def test_yaml():
    import yaml
    with open('config.yml') as cfg:
        logging_cfg = yaml.safe_load(stream=cfg)
    logging.config.dictConfig(logging_cfg['logging'])
    logger = logging.getLogger()
    test_logger(logger)


if __name__ == '__main__':
    # test_normal()
    test_yaml()
