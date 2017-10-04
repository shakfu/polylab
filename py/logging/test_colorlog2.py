"""
Bootstrap for console and file logging configuration.
"""

import os
import logging
import logging.config
import yaml

__version__ = '1.4.0'

awesome_logging_config = \
    """
    version: 1
    disable_existing_loggers: False

    formatters:
        console:
            format : "[%(levelname)-7s] %(message)s (%(name)s)"
            datefmt: "%H:%M:%S"

        file:
            format : >-
                [%(asctime)s]%(thread)-5d %(name)-40s
                %(levelname)-8s %(message)s
            datefmt: "%Y-%m-%dT%H:%M:%S"

        colored:
            format : >-
                [%(log_color)s%(levelname)-8s%(reset)s] %(message_log_color)s
                %(message)s%(reset)s %(name_log_color)s(%(name)s)
            datefmt: "%H:%M:%S"
            ()     : colorlog.ColoredFormatter

            log_colors:
                DEBUG   : white
                INFO    : bold_green
                WARNING : bold_yellow
                ERROR   : bold_red
                CRITICAL: bold_white,bg_red

            secondary_log_colors:
                message:
                    INFO    : bold_white
                    WARNING : bold_yellow
                    ERROR   : bold_red
                    CRITICAL: bold_red

                name:
                    DEBUG   : purple
                    INFO    : purple
                    WARNING : purple
                    ERROR   : purple
                    CRITICAL: purple

    handlers:
        console:
            level    : DEBUG
            class    : logging.StreamHandler
            formatter: colored
            stream   : ext://sys.stdout

        file:
            level    : DEBUG
            class    : logging.handlers.TimedRotatingFileHandler
            formatter: file
            when     : midnight
            filename : logs/log.log
            encoding : utf8

    loggers:
        custom_module:
            handlers: [file]
            level: WARNING

    root:
        handlers: [console, file]
        level: DEBUG
    """

def test_logger(logger):
    logger.debug('a debug message')
    logger.info('an info message')
    logger.warning('a warning message')
    logger.error('an error message')
    logger.critical('a critical message')


os.makedirs('logs', exist_ok=True)

try:
    config = yaml.load(awesome_logging_config)
    logging.config.dictConfig(config)
    logger = logging.getLogger()
    test_logger(logger)
except Exception:
    raise ValueError("Couldn't import logging settings from yaml")
