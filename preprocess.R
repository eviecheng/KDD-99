setwd("~/KDD-99")
data = read.csv("kddcup.data_10_percent.txt", header = FALSE)
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

# remove variables without much information
data$urgent = NULL
data$is_hot_login = NULL
data$num_outbound_cmds = NULL

# su_attempted - 1 if "su root" command attempted; 0 otherwise
data$su_attempted[data$su_attempted == "2"] = "1"

attach(data)

# Plots
# pairs(result~colnames(data)[1:5] , data = data)
# pairs(result~duration+protocol_type , data = data)
boxplot(duration~factor(result), data = data, xlab = "Result", ylab = "Duration")
boxplot(src_bytes~factor(result), data = data, xlab = "Result", ylab = "Src_bytes")
boxplot(dst_bytes~factor(result), data = data, xlab = "Result", ylab = "Dst_bytes")
plot(protocol_type, result, xlab = "Protocol type", ylab = "Result")
plot(service, result, xlab = "Service", ylab = "Result")
plot(flag, result, xlab = "Flag", ylab = "Result")
plot(land, result, xlab = "Land", ylab = "Result")
plot(wrong, result, xlab = "Wrong", ylab = "Result")
#
boxplot(hot~factor(result), data = data, xlab = "Result", ylab = "Hot")
boxplot(num_failed_logins~factor(result), data = data, xlab = "Result", ylab = "Num_failed_logins")
boxplot(num_compromised~factor(result), data = data, xlab = "Result", ylab = "Num_compromised")
boxplot(as.numeric(num_root)~factor(result), data = data, xlab = "Result", ylab = "num_root")
boxplot(as.numeric(num_file_creations)~factor(result), data = data, xlab = "Result", ylab = "num_file_creations")
boxplot(as.numeric(num_shells)~factor(result), data = data, xlab = "Result", ylab = "num_shells")
boxplot(num_access_files~factor(result), data = data, xlab = "Result", ylab = "num_access_files")
plot(factor(logged_in), result, xlab = "Logged_in", ylab = "Result")
plot(factor(root_shell), result, xlab = "Root_shell", ylab = "Result")
plot(factor(su_attempted), result, xlab = "su_attempted", ylab = "Result")
plot(factor(is_guest_login), result, xlab = "is_guest_login", ylab = "Result")
#
boxplot(count~factor(result), data = data, xlab = "Result", ylab = "count")
boxplot(serror_rate~factor(result), data = data, xlab = "Result", ylab = "serror_rate")
boxplot(rerror_rate~factor(result), data = data, xlab = "Result", ylab = "rerror_rate")
boxplot(same_srv_rate~factor(result), data = data, xlab = "Result", ylab = "same_srv_rate")
boxplot(diff_srv_rate~factor(result), data = data, xlab = "Result", ylab = "diff_srv_rate")
boxplot(srv_count~factor(result), data = data, xlab = "Result", ylab = "srv_count")
boxplot(srv_serror_rate~factor(result), data = data, xlab = "Result", ylab = "srv_serror_rate")
boxplot(srv_rerror_rate~factor(result), data = data, xlab = "Result", ylab = "srv_rerror_rate")
boxplot(srv_diff_host_rate~factor(result), data = data, xlab = "Result", ylab = "srv_diff_host_rate")

table(result)
pie(table(data$result)/nrow(data))

saveRDS(data, "data_10_percent_processed.rds")