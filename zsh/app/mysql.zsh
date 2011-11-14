# Create a new super user in mySQL
mysql_create_super_user () {
  if [ $# -ne 2 ]; then
    echo "Usage: $0 username password"
    return 1
  fi
  
  for user in "'${1}'@'localhost'" "'${1}'@'%'"; do
    echo CREATE USER "${user}" IDENTIFIED BY "'${2}'" | mysql -u root
    echo GRANT ALL PRIVILEGES ON "*.*" TO "${user}" WITH GRANT OPTION | mysql -u root
  done
}
