# alias ssscreen="env | grep SSH | grep -v ' ' > ~/.ssh-agent.txt && screen"
alias ressh='for f in $(find ~/.ssvars -name '"'"'*.sh'"'"'); do source ${f}; done'
