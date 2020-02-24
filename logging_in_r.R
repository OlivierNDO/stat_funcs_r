# Script config
library(logging)
config_log_folder = 'C:/Users/Nicholas.Olivier/Documents/log_archive/'
config_log_file = 'abc_model.log'
config_logger_name = 'abc_model'


# Setting up logger
logging::basicConfig()

logging::addHandler(logging::writeToFile,
                    logger = config_logger_name,
                    file = paste0(config_log_folder, config_log_file))



# Examples of messages to add in logging text file
logging::loginfo("hello world", logger = paste0(config_logger_name, '.module'))

logging::logwarn('this is a warning', logger = paste0(config_logger_name, '.module'))

logging::logerror('this is an error', logger = paste0(config_logger_name, '.module'))

logging::logReset()