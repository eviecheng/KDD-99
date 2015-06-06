data = read.csv("kddcup.data_10_percent.txt")
colnames(data) = c("duration", "protocol_type", "service", "flag", "src_bytes", "dst_bytes", "land", "wrong_fragment", "urgent", "hot", "num_failed_logins", "logged_in", "num_compromised", "root_shell", "su_attempted", "num_root", "num_file_creations", "num_shells", "num_access_files", "num_outbound_cmds", "is_hot_login", "is_guest_login", "count", "srv_count", "serror_rate", "srv_serror_rate", "rerror_rate", "srv_rerror_rate", "same_srv_rate", "diff_srv_rate", "srv_diff_host_rate", "dst_host_count", "dst_host_srv_count", "dst_host_same_srv_rate", "dst_host_diff_srv_rate", "dst_host_same_src_port_rate", "dst_host_srv_diff_host_rate", "dst_host_serror_rate", "dst_host_srv_serror_rate", "dst_host_rerror_rate", "dst_host_srv_rerror_rate", "result")

### preprocessing
# result
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
data$result = as.factor(data$result)

# wrong_fragment from contin. variable to discreet variable
data$wrong = ifelse(data$wrong_fragment == 0, "No", "Yes")
data$wrong_fragment = NULL
data = data[, c(1:7,42,8:41)]

# service

# src_bytes
# data = data[!(data$src_bytes == max(data$src_bytes)),] # remove the data with abnormal src_bytes

# remove variables without much information
data$urgent = NULL
data$is_hot_login = NULL
data$num_outbound_cmds = NULL

