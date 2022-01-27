# alias ssscreen="env | grep SSH | grep -v ' ' > ~/.ssh-agent.txt && screen"
alias ls='ls --color=auto'
alias cp='cp --reflink=auto'
alias ressh='for f in $(find ~/.ssvars -name '"'"'*.sh'"'"'); do source ${f}; done'
