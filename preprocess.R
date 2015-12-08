### preprocessing for 'kddcup.data_10_percent'

setwd("~/KDD-99")
data = read.csv("kddcup.data_10_percent.txt", header = FALSE)
colnames(data) = c("duration", "protocol_type", "service", "flag", "src_bytes", "dst_bytes", "land", "wrong_fragment", "urgent", "hot", "num_failed_logins", "logged_in", "num_compromised", "root_shell", "su_attempted", "num_root", "num_file_creations", "num_shells", "num_access_files", "num_outbound_cmds", "is_hot_login", "is_guest_login", "count", "srv_count", "serror_rate", "srv_serror_rate", "rerror_rate", "srv_rerror_rate", "same_srv_rate", "diff_srv_rate", "srv_diff_host_rate", "dst_host_count", "dst_host_srv_count", "dst_host_same_srv_rate", "dst_host_diff_srv_rate", "dst_host_same_src_port_rate", "dst_host_srv_diff_host_rate", "dst_host_serror_rate", "dst_host_srv_serror_rate", "dst_host_rerror_rate", "dst_host_srv_rerror_rate", "result")

# features preprocess
data$duration = as.numeric(as.character(data$duration))
data$protocol_type = factor(data$protocol_type)
data$service = factor(data$service)
data$flag = factor(data$flag)
data$src_bytes = as.numeric(as.character(data$src_bytes))
data$dst_bytes = as.numeric(as.character(data$dst_bytes))
data$land = factor(data$land)
data$wrong_fragment = as.numeric(as.character(data$wrong_fragment))
data$urgent = as.numeric(as.character(data$urgent))
data$hot = as.numeric(as.character(data$hot))
data$num_failed_logins = as.numeric(as.character(data$num_failed_logins))
data$logged_in = factor(data$logged_in)
data$num_compromised = as.numeric(as.character(data$num_compromised))
data$root_shell = factor(data$root_shell)
data$su_attempted = factor(data$su_attempted)
data$num_root = as.numeric(as.character(data$num_root))
data$num_file_creations = as.numeric(as.character(data$num_file_creations))
data$num_shells = as.numeric(as.character(data$num_shells))
data$num_access_files = as.numeric(as.character(data$num_access_files))
data$num_outbound_cmds = as.numeric(as.character(data$num_outbound_cmds))
data$is_hot_login = factor(data$is_hot_login)
data$is_guest_login = factor(data$is_guest_login)
data$count = as.numeric(as.character(data$count))
data$srv_count = as.numeric(as.character(data$srv_count))
data$serror_rate = as.numeric(as.character(data$serror_rate))
data$srv_serror_rate = as.numeric(as.character(data$srv_serror_rate))
data$rerror_rate = as.numeric(as.character(data$rerror_rate))        
data$srv_rerror_rate = as.numeric(as.character(data$srv_rerror_rate))
data$same_srv_rate = as.numeric(as.character(data$same_srv_rate)) 
data$diff_srv_rate = as.numeric(as.character(data$diff_srv_rate))           
data$srv_diff_host_rate = as.numeric(as.character(data$srv_diff_host_rate))
data$dst_host_count = as.numeric(as.character(data$dst_host_count)) 
data$dst_host_srv_count = as.numeric(as.character(data$dst_host_srv_count))              
data$dst_host_same_srv_rate = as.numeric(as.character(data$dst_host_same_srv_rate))
data$dst_host_diff_srv_rate = as.numeric(as.character(data$dst_host_diff_srv_rate)) 
data$dst_host_same_src_port_rate = as.numeric(as.character(data$dst_host_same_src_port_rate))                 
data$dst_host_srv_diff_host_rate = as.numeric(as.character(data$dst_host_srv_diff_host_rate))
data$dst_host_serror_rate = as.numeric(as.character(data$dst_host_serror_rate)) 
data$dst_host_srv_serror_rate = as.numeric(as.character(data$dst_host_srv_serror_rate))              
data$dst_host_rerror_rate = as.numeric(as.character(data$dst_host_rerror_rate)) 
data$dst_host_srv_rerror_rate = as.numeric(as.character(data$dst_host_srv_rerror_rate))       

# transform continuous feature 'wrong_fragment' into categorical feature 'wrong'
data$wrong = ifelse(data$wrong_fragment == 0, 0, 1)
data$wrong_fragment = NULL
data$wrong = factor(data$wrong)
data = data[, c(1:7,42,8:41)]

# su_attempted - 1 if "su root" command attempted; 0 otherwise
# > table(data$su_attempted)

#      0      1      2 
# 494008      6      6 
data$su_attempted[data$su_attempted == "2"] = "1"

# remove features without additional information - 'is_hot_login' and 'num_outbound_cmds'
# > table(data$is_hot_login)

#      0 
# 494020 
data$is_hot_login = NULL

# plot(data$num_outbound_cmds)
data$num_outbound_cmds = NULL


# result preprocess
data$result = as.character(data$result)
data$result[data$result == "ipsweep."] = "probe"
data$result[data$result == "portsweep."] = "probe"
data$result[data$result == "nmap."] = "probe"
data$result[data$result == "satan."] = "probe"
data$result[data$result == "buffer_overflow."] = "u2r"
data$result[data$result == "loadmodule."] = "u2r"
data$result[data$result == "perl."] = "u2r"
data$result[data$result == "rootkit."] = "u2r"
data$result[data$result == "back."] = "dos"
data$result[data$result == "land."] = "dos"
data$result[data$result == "neptune."] = "dos"
data$result[data$result == "pod."] = "dos"
data$result[data$result == "smurf."] = "dos"
data$result[data$result == "teardrop."] = "dos"
data$result[data$result == "ftp_write."] = "r2l"
data$result[data$result == "guess_passwd."] = "r2l"
data$result[data$result == "imap."] = "r2l"
data$result[data$result == "multihop."] = "r2l"
data$result[data$result == "phf."] = "r2l"
data$result[data$result == "spy."] = "r2l"
data$result[data$result == "warezclient."] = "r2l"
data$result[data$result == "warezmaster."] = "r2l"
data$result[data$result == "normal."] = "normal"
data = data[!(data$result=="0.00"),]
data = data[!(data$result==""),]
data$result = factor(data$result)

saveRDS(data, "data_10_percent_processed.rds")


### additional preprocess for feature 'service' for tree model
service = as.character(service)
service_prob = matrix(0, nrow = 66, ncol = 5)
for(i in 1:66){
  service_prob[i, ] = as.vector(table(result[service == levels(service)[i]])) / sum(as.vector(table(result[service == levels(service)[i]])))
}
service_cor = matrix(0, nrow = 66, ncol = 66)
for(i in 1:66){
  for (j in i:66){
    service_cor[i, j] = cor(service_prob[i, ], service_prob[j, ], method = "pearson")
  }
}
service_cor_temp = matrix(0, nrow = 66, ncol = 66)
service_cor_temp[service_cor > 0.95] = service_cor[service_cor > 0.95]

# group 'service 'into groups 1-7
levels(data$service)[which(service_cor_temp[1, ] > 0)]
levels(data$service)[which(service_cor_temp[2, ] > 0)]
levels(data$service)[which(service_cor_temp[9, ] > 0)]
levels(data$service)[which(service_cor_temp[11, ] > 0)]
levels(data$service)[which(service_cor_temp[16, ] > 0)]
levels(data$service)[which(service_cor_temp[42, ] > 0)]
levels(data$service)[which(service_cor_temp[55, ] > 0)]

data$service = as.character(data$service)
data$service[data$service == "auth"] = 1
data$service[data$service == "finger"] = 1
data$service[data$service == "bgp"] = 2
data$service[data$service == "courier"] = 2
data$service[data$service == "csnet_ns"] = 2
data$service[data$service == "ctf"] = 2
data$service[data$service == "daytime"] = 2
data$service[data$service == "discard"] = 2
data$service[data$service == "domain"] = 2
data$service[data$service == "echo"] = 2
data$service[data$service == "ecr_i"] = 2
data$service[data$service == "efs"] = 2
data$service[data$service == "exec"] = 2
data$service[data$service == "gopher"] = 2
data$service[data$service == "hostnames"] = 2
data$service[data$service == "http_443"] = 2
data$service[data$service == "imap4"] = 2
data$service[data$service == "iso_tsap"] = 2
data$service[data$service == "klogin"] = 2
data$service[data$service == "kshell"] = 2
data$service[data$service == "ldap"] = 2
data$service[data$service == "link"] = 2
data$service[data$service == "login"] = 2
data$service[data$service == "mtp"] = 2
data$service[data$service == "name"] = 2
data$service[data$service == "netbios_dgm"] = 2
data$service[data$service == "netbios_ns"] = 2
data$service[data$service == "netbios_ssn"] = 2
data$service[data$service == "netstat"] = 2
data$service[data$service == "nnsp"] = 2
data$service[data$service == "nntp"] = 2
data$service[data$service == "pop_2"] = 2
data$service[data$service == "printer"] = 2
data$service[data$service == "private"] = 2
data$service[data$service == "remote_job"] = 2
data$service[data$service == "rje"] = 2
data$service[data$service == "shell"] = 2
data$service[data$service == "sql_net"] = 2
data$service[data$service == "ssh"] = 2
data$service[data$service == "sunrpc"] = 2
data$service[data$service == "supdup"] = 2
data$service[data$service == "systat"] = 2
data$service[data$service == "uucp"] = 2
data$service[data$service == "uucp_path"] = 2
data$service[data$service == "vmnet"] = 2
data$service[data$service == "whois"] = 2
data$service[data$service == "Z39_50"] = 2
data$service[data$service == "domain_u"] = 3
data$service[data$service == "ftp_data"] = 3
data$service[data$service == "http"] = 3
data$service[data$service == "IRC"] = 3
data$service[data$service == "ntp_u"] = 3
data$service[data$service == "other"] = 3
data$service[data$service == "red_i"] = 3
data$service[data$service == "smtp"] = 3
data$service[data$service == "tftp_u"] = 3
data$service[data$service == "urh_i"] = 3
data$service[data$service == "urp_i"] = 3
data$service[data$service == "X11"] = 3
data$service[data$service == "eco_i"] = 4
data$service[data$service == "pm_dump"] = 4
data$service[data$service == "ftp"] = 5
data$service[data$service == "pop_3"] = 6
data$service[data$service == "tim_i"] = 6
data$service[data$service == "time"] = 6
data$service[data$service == "telnet"] = 7
data$service = as.factor(data$service)

saveRDS(data, "10_percent_service_processed.rds")