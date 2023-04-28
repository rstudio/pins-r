awk ' { system("useradd -m -s /bin/bash "$1); system("echo \""$1":"$2"\" | chpasswd"); system("id "$1) } ' /etc/users.txt
