#!/usr/bin/env bash
#
# Based on a template by BASH3 Boilerplate v2.3.0
# http://bash3boilerplate.sh/#authors
#
# The MIT License (MIT)
# Copyright (c) 2013 Kevin van Zonneveld and contributors
# You are not obligated to bundle the LICENSE file with your b3bp projects as long
# as you leave these references intact in the header comments of your source files.

# Exit on error. Append "|| true" if you expect an error.
set -o errexit
# Exit on error inside any functions or subshells.
set -o errtrace
# Do not allow use of undefined vars. Use ${VAR:-} to use an undefined VAR
set -o nounset
# Catch the error in case mysqldump fails (but gzip succeeds) in `mysqldump |gzip`
set -o pipefail
# Turn on traces, useful while debugging but commented out by default
# set -o xtrace

# Define the environment variables (and their defaults) that this script depends on
LOG_LEVEL="${LOG_LEVEL:-6}" # 7 = debug -> 0 = emergency
NO_COLOR="${NO_COLOR:-}"    # true = disable color. otherwise autodetected


### Functions
##############################################################################

function __b3bp_log () {
  local log_level="${1}"
  shift

  # shellcheck disable=SC2034
  local color_debug="\x1b[35m"
  # shellcheck disable=SC2034
  local color_info="\x1b[32m"
  # shellcheck disable=SC2034
  local color_notice="\x1b[34m"
  # shellcheck disable=SC2034
  local color_warning="\x1b[33m"
  # shellcheck disable=SC2034
  local color_error="\x1b[31m"
  # shellcheck disable=SC2034
  local color_critical="\x1b[1;31m"
  # shellcheck disable=SC2034
  local color_alert="\x1b[1;33;41m"
  # shellcheck disable=SC2034
  local color_emergency="\x1b[1;4;5;33;41m"

  local colorvar="color_${log_level}"

  local color="${!colorvar:-${color_error}}"
  local color_reset="\x1b[0m"

  if [[ "${NO_COLOR:-}" = "true" ]] || [[ "${TERM:-}" != "xterm"* ]] || [[ ! -t 2 ]]; then
    if [[ "${NO_COLOR:-}" != "false" ]]; then
      # Don't use colors on pipes or non-recognized terminals
      color=""; color_reset=""
    fi
  fi

  # all remaining arguments are to be printed
  local log_line=""

  while IFS=$'\n' read -r log_line; do
    echo -e "$(date -u +"%Y-%m-%d %H:%M:%S UTC") ${color}$(printf "[%9s]" "${log_level}")${color_reset} ${log_line}" 1>&2
  done <<< "${@:-}"
}

function emergency () {                                __b3bp_log emergency "${@}"; exit 1; }
function alert ()     { [[ "${LOG_LEVEL:-0}" -ge 1 ]] && __b3bp_log alert "${@}"; true; }
function critical ()  { [[ "${LOG_LEVEL:-0}" -ge 2 ]] && __b3bp_log critical "${@}"; true; }
function error ()     { [[ "${LOG_LEVEL:-0}" -ge 3 ]] && __b3bp_log error "${@}"; true; }
function warning ()   { [[ "${LOG_LEVEL:-0}" -ge 4 ]] && __b3bp_log warning "${@}"; true; }
function notice ()    { [[ "${LOG_LEVEL:-0}" -ge 5 ]] && __b3bp_log notice "${@}"; true; }
function info ()      { [[ "${LOG_LEVEL:-0}" -ge 6 ]] && __b3bp_log info "${@}"; true; }
function debug ()     { [[ "${LOG_LEVEL:-0}" -ge 7 ]] && __b3bp_log debug "${@}"; true; }

function help () {
  echo "" 1>&2
  echo " ${*}" 1>&2
  echo "" 1>&2
  echo "  ${__usage:-No usage available}" 1>&2
  echo "" 1>&2

  if [[ "${__helptext:-}" ]]; then
    echo " ${__helptext}" 1>&2
    echo "" 1>&2
  fi

  exit 1
}


### Parse commandline options
##############################################################################

# Commandline options. This defines the usage page, and is used to parse cli
# opts & defaults from. The parsing is unforgiving so be precise in your syntax
# - A short option must be preset for every long option; but every short option
#   need not have a long option
# - `--` is respected as the separator between options and arguments
# - We do not bash-expand defaults, so setting '~/app' as a default will not resolve to ${HOME}.
#   you can use bash variables to work around this (so use ${HOME} instead)

# shellcheck disable=SC2015
[[ "${__usage+x}" ]] || read -r -d '' __usage <<-'EOF' || true # exits non-zero when EOF encountered
  -v               Enable verbose mode, print script as it is executed
  -d --debug       Enables debug mode
  -h --help        This page
  -n --no-color    Disable color output
  -1 --one         Do just one thing
EOF

# shellcheck disable=SC2015
[[ "${__helptext+x}" ]] || read -r -d '' __helptext <<-'EOF' || true # exits non-zero when EOF encountered
 This is Bash3 Boilerplate's help text. Feel free to add any description of your
 program or elaborate more on command-line arguments. This section is not
 parsed and will be added as-is to the help.
EOF

# Translate usage string -> getopts arguments, and set $arg_<flag> defaults
while read -r __b3bp_tmp_line; do
  if [[ "${__b3bp_tmp_line}" =~ ^- ]]; then
    # fetch single character version of option string
    __b3bp_tmp_opt="${__b3bp_tmp_line%% *}"
    __b3bp_tmp_opt="${__b3bp_tmp_opt:1}"

    # fetch long version if present
    __b3bp_tmp_long_opt=""

    if [[ "${__b3bp_tmp_line}" = *"--"* ]]; then
      __b3bp_tmp_long_opt="${__b3bp_tmp_line#*--}"
      __b3bp_tmp_long_opt="${__b3bp_tmp_long_opt%% *}"
    fi

    # map opt long name to+from opt short name
    printf -v "__b3bp_tmp_opt_long2short_${__b3bp_tmp_long_opt//-/_}" '%s' "${__b3bp_tmp_opt}"
    printf -v "__b3bp_tmp_opt_short2long_${__b3bp_tmp_opt}" '%s' "${__b3bp_tmp_long_opt//-/_}"

    # check if option takes an argument
    if [[ "${__b3bp_tmp_line}" =~ \[.*\] ]]; then
      __b3bp_tmp_opt="${__b3bp_tmp_opt}:" # add : if opt has arg
      __b3bp_tmp_init=""  # it has an arg. init with ""
      printf -v "__b3bp_tmp_has_arg_${__b3bp_tmp_opt:0:1}" '%s' "1"
    elif [[ "${__b3bp_tmp_line}" =~ \{.*\} ]]; then
      __b3bp_tmp_opt="${__b3bp_tmp_opt}:" # add : if opt has arg
      __b3bp_tmp_init=""  # it has an arg. init with ""
      # remember that this option requires an argument
      printf -v "__b3bp_tmp_has_arg_${__b3bp_tmp_opt:0:1}" '%s' "2"
    else
      __b3bp_tmp_init="0" # it's a flag. init with 0
      printf -v "__b3bp_tmp_has_arg_${__b3bp_tmp_opt:0:1}" '%s' "0"
    fi
    __b3bp_tmp_opts="${__b3bp_tmp_opts:-}${__b3bp_tmp_opt}"
  fi

  [[ "${__b3bp_tmp_opt:-}" ]] || continue

  if [[ "${__b3bp_tmp_line}" =~ (^|\.\ *)Default= ]]; then
    # ignore default value if option does not have an argument
    __b3bp_tmp_varname="__b3bp_tmp_has_arg_${__b3bp_tmp_opt:0:1}"

    if [[ "${!__b3bp_tmp_varname}" != "0" ]]; then
      __b3bp_tmp_init="${__b3bp_tmp_line##*Default=}"
      __b3bp_tmp_re='^"(.*)"$'
      if [[ "${__b3bp_tmp_init}" =~ ${__b3bp_tmp_re} ]]; then
        __b3bp_tmp_init="${BASH_REMATCH[1]}"
      else
        __b3bp_tmp_re="^'(.*)'$"
        if [[ "${__b3bp_tmp_init}" =~ ${__b3bp_tmp_re} ]]; then
          __b3bp_tmp_init="${BASH_REMATCH[1]}"
        fi
      fi
    fi
  fi

  if [[ "${__b3bp_tmp_line}" =~ (^|\.\ *)Required\. ]]; then
    # remember that this option requires an argument
    printf -v "__b3bp_tmp_has_arg_${__b3bp_tmp_opt:0:1}" '%s' "2"
  fi

  printf -v "arg_${__b3bp_tmp_opt:0:1}" '%s' "${__b3bp_tmp_init}"
done <<< "${__usage:-}"

# run getopts only if options were specified in __usage
if [[ "${__b3bp_tmp_opts:-}" ]]; then
  # Allow long options like --this
  __b3bp_tmp_opts="${__b3bp_tmp_opts}-:"

  # Reset in case getopts has been used previously in the shell.
  OPTIND=1

  # start parsing command line
  set +o nounset # unexpected arguments will cause unbound variables
                 # to be dereferenced
  # Overwrite $arg_<flag> defaults with the actual CLI options
  while getopts "${__b3bp_tmp_opts}" __b3bp_tmp_opt; do
    [[ "${__b3bp_tmp_opt}" = "?" ]] && help "Invalid use of script: ${*} "

    if [[ "${__b3bp_tmp_opt}" = "-" ]]; then
      # OPTARG is long-option-name or long-option=value
      if [[ "${OPTARG}" =~ .*=.* ]]; then
        # --key=value format
        __b3bp_tmp_long_opt=${OPTARG/=*/}
        # Set opt to the short option corresponding to the long option
        __b3bp_tmp_varname="__b3bp_tmp_opt_long2short_${__b3bp_tmp_long_opt//-/_}"
        printf -v "__b3bp_tmp_opt" '%s' "${!__b3bp_tmp_varname}"
        OPTARG=${OPTARG#*=}
      else
        # --key value format
        # Map long name to short version of option
        __b3bp_tmp_varname="__b3bp_tmp_opt_long2short_${OPTARG//-/_}"
        printf -v "__b3bp_tmp_opt" '%s' "${!__b3bp_tmp_varname}"
        # Only assign OPTARG if option takes an argument
        __b3bp_tmp_varname="__b3bp_tmp_has_arg_${__b3bp_tmp_opt}"
        printf -v "OPTARG" '%s' "${@:OPTIND:${!__b3bp_tmp_varname}}"
        # shift over the argument if argument is expected
        ((OPTIND+=__b3bp_tmp_has_arg_${__b3bp_tmp_opt}))
      fi
      # we have set opt/OPTARG to the short value and the argument as OPTARG if it exists
    fi
    __b3bp_tmp_varname="arg_${__b3bp_tmp_opt:0:1}"
    __b3bp_tmp_default="${!__b3bp_tmp_varname}"

    __b3bp_tmp_value="${OPTARG}"
    if [[ -z "${OPTARG}" ]] && [[ "${__b3bp_tmp_default}" = "0" ]]; then
      __b3bp_tmp_value="1"
    fi

    printf -v "${__b3bp_tmp_varname}" '%s' "${__b3bp_tmp_value}"
    debug "cli arg ${__b3bp_tmp_varname} = (${__b3bp_tmp_default}) -> ${!__b3bp_tmp_varname}"
  done
  set -o nounset # no more unbound variable references expected

  shift $((OPTIND-1))

  if [[ "${1:-}" = "--" ]] ; then
    shift
  fi
fi


### Automatic validation of required option arguments
##############################################################################

for __b3bp_tmp_varname in ${!__b3bp_tmp_has_arg_*}; do
  # validate only options which required an argument
  [[ "${!__b3bp_tmp_varname}" = "2" ]] || continue

  __b3bp_tmp_opt_short="${__b3bp_tmp_varname##*_}"
  __b3bp_tmp_varname="arg_${__b3bp_tmp_opt_short}"
  [[ "${!__b3bp_tmp_varname}" ]] && continue

  __b3bp_tmp_varname="__b3bp_tmp_opt_short2long_${__b3bp_tmp_opt_short}"
  printf -v "__b3bp_tmp_opt_long" '%s' "${!__b3bp_tmp_varname}"
  [[ "${__b3bp_tmp_opt_long:-}" ]] && __b3bp_tmp_opt_long=" (--${__b3bp_tmp_opt_long//_/-})"

  help "Option -${__b3bp_tmp_opt_short}${__b3bp_tmp_opt_long:-} requires an argument"
done


### Cleanup Environment variables
##############################################################################

for __tmp_varname in ${!__b3bp_tmp_*}; do
  unset -v "${__tmp_varname}"
done

unset -v __tmp_varname


### Externally supplied __usage. Nothing else to do here
##############################################################################

if [[ "${__b3bp_external_usage:-}" = "true" ]]; then
  unset -v __b3bp_external_usage
  return
fi


### Signal trapping and backtracing
##############################################################################

RESET_TERMINAL=""
function __b3bp_cleanup_before_exit () {
    if test -z "$RESET_TERMINAL"; then
	info Script finished
    else
	warning "Environment variables have been reset, please logout and log back in"
    fi
}
trap __b3bp_cleanup_before_exit EXIT

# requires `set -o errtrace`
__b3bp_err_report() {
    local error_code
    error_code=${?}
    error "Error in ${__file} in function ${1} on line ${2}"
    exit ${error_code}
}
# Uncomment the following line for always providing an error backtrace
# trap '__b3bp_err_report "${FUNCNAME:-.}" ${LINENO}' ERR


### Command-line argument switches (like -d for debugmode, -h for showing helppage)
##############################################################################

# debug mode
if [[ "${arg_d:?}" = "1" ]]; then
  set -o xtrace
  LOG_LEVEL="7"
  # Enable error backtracing
  trap '__b3bp_err_report "${FUNCNAME:-.}" ${LINENO}' ERR
fi

# verbose mode
if [[ "${arg_v:?}" = "1" ]]; then
  set -o verbose
fi

# no color mode
if [[ "${arg_n:?}" = "1" ]]; then
  NO_COLOR="true"
fi

# help mode
if [[ "${arg_h:?}" = "1" ]]; then
  # Help exists with code 1
  help "Help using ${0}"
fi

function yesno() {
	read -p "$1 " YN

	[ "$YN" == "y" ]
}

function not_installed_force() {
    app_name="$1"

    if ! command -v $1 > /dev/null; then
	info "$1 not found"
	return 0
    else
	info "$1 installed"
	return 1
    fi
}

function not_installed() {
    app_name="$1"

    if ! command -v $1 > /dev/null; then
	info "$1 not found"
	
	yesno "Install $1?"
    else
	info "$1 installed"
	return 1
    fi
}

UPDATED=""
function update_check {
    if test -z "$UPDATED"; then
	info "Running apt-get update"
	sudo apt-get update
    fi

    UPDATED=1
}

function install_application {
    update_check
    sudo apt-get install -y $1
}

function install_docker {
    info "Installing Docker..."
    update_check

    sudo apt-get install -y \
	 apt-transport-https \
	 ca-certificates \
	 curl \
	 gnupg2 \
	 software-properties-common

    info "Adding gpg key for docker repository"
    
    # Add the gpg key if it;s not there
    curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add -

    # Add the docker repository
    sudo add-apt-repository \
	 "deb [arch=amd64] https://download.docker.com/linux/debian \
     $(lsb_release -cs) \
     stable"

    info "Installing docker-ce"
    # Update and add docker-ce
    sudo apt-get update
    sudo apt-get install -y docker-ce

    info "Installing docker-compose"
    # Install docker-compose
    sudo curl -L "https://github.com/docker/compose/releases/download/1.24.0/docker-compose-$(uname -s)-$(uname -m)" \
	 -o /usr/local/bin/docker-compose
    
    # Add yourself to the docker group if you aren't already
    # sudo groupadd docker
    sudo usermod -aG docker $USER
}

function install_rbenv() {
    RESET_TERMINAL=1
    update_check
    info Installing rbenv

    sudo apt-get install -y build-essential libssl-dev libreadline-dev zlib1g-dev

    echo 'export PATH=$PATH:$HOME/.rbenv/bin' >> $HOME/.profile
    echo 'eval "$(rbenv init -)"' >> $HOME/.profile
    export PATH=$PATH:$HOME/.rbenv/bin

    (
	curl -fsSL https://github.com/rbenv/rbenv-installer/raw/master/bin/rbenv-installer | bash
    )
    
    eval "$(rbenv init -)"
    
    warning "Be sure to logout and log back in, or source ~/.profile"
    source ~/.profile
    rbenv install 2.5.5
    rbenv global 2.5.5
}

function install_nvm() {
    RESET_TERMINAL=1
    info Installing nvm

    curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.34.0/install.sh | (bash)

    export NVM_DIR="$HOME/.config"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

    warning "Be sure to loging and log back in, or source ~/.bashrc"
    nvm install 10
    nvm global 10
}

function install_go() {
    RESET_TERMINAL=1
    info Installing go
    (
	cd /tmp
	wget https://dl.google.com/go/go1.12.1.linux-amd64.tar.gz
	sudo tar -C /usr/local -xzf go1.12.1.linux-amd64.tar.gz
	echo 'export PATH=$PATH:/usr/local/go/bin:$HOME/go/bin' >> $HOME/.profile
    )
    export PATH=$PATH:/usr/local/go/bin:$HOME/go/bin
    source ~/.profile
}

function install_hugo() {
    info Installing hugo
    update_check

    (
	HUGO_VERSION=0.56.3
	HUGO_FILENAME=hugo_${HUGO_VERSION}_Linux-64bit.deb
	cd /tmp
	wget https://github.com/gohugoio/hugo/releases/download/v${HUGO_VERSION}/${HUGO_FILENAME}
	sudo apt install -u ./${HUGO_FILENAME}
    )
    info $(hugo version)
}

function install_atom() {
    info Installing atom
    update_check
    
    (
	cd /tmp
	sudo apt-get install -y wget
	wget https://atom.io/download/deb
	mv deb atom.deb
	sudo apt install -y ./atom.deb
    )
}

function install_gcloud() {
    info Installing gcloud
    
    # Create environment variable for correct distribution
    export CLOUD_SDK_REPO="cloud-sdk-$(lsb_release -c -s)"

    # Add the Cloud SDK distribution URI as a package source
    echo "deb http://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list

    # Import the Google Cloud Platform public key
    curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key add -

    # Update the package list and install the Cloud SDK
    sudo apt-get update && sudo apt-get install -y google-cloud-sdk
}


function install_heroku() {
    info Installing heroku
    curl https://cli-assets.heroku.com/install-ubuntu.sh | sh
}

function install_terraform() {
    info Installing terraform
    (
	cd /tmp
	wget https://releases.hashicorp.com/terraform/0.12.18/terraform_0.12.18_linux_amd64.zip
	unzip terraform_0.12.18_linux_amd64.zip
	sudo mv terraform /usr/local/bin
    )
}

function install_packer() {
    info Installing packer
    (
	cd /tmp
	wget https://releases.hashicorp.com/packer/1.5.1/packer_1.5.1_linux_amd64.zip
	unzip packer_1.5.1_linux_amd64.zip
	sudo mv packer /usr/local/bin
    )
}

function install_ansible {
    info Installing ansible

    (
	cd /tmp
	curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
	python get-pip.py --user
	RESET_TERMINAL=1
	info Adding .local/bin to the path, please source .profile
	echo "export PATH=$HOME/.local/bin:$PATH" >> ~/.profile
	export PATH=$HOME/.local/bin:$PATH
	pip install --user ansible
    )
}

function install_steam() {
    info Installing steam
    update_check

    echo 'deb http://httpredir.debian.org/debian/ jessie main contrib non-free' | sudo tee -a /etc/apt/sources.list
    sudo dpkg --add-architecture i386
    sudo apt update
    sudo apt install -y steam
}

## Actual setup scripts

info "Begin setup"

not_installed_force gcc && install_application "build-essential libssl-dev"

not_installed_force git && install_application git

if ! git config --get user.email > /dev/null; then
  read -p "Email Address (for git): " gitemail
  git config --global user.email "$gitemail"
fi
gitemail=$(git config --get user.email)
info "Git Email: $gitemail"

if ! git config --get user.name > /dev/null; then
  read -p "Full name (for git)    : " gitname
  git config --global user.name "$gitname"
fi
gitname=$(git config --get user.name)
info "Git Name: $gitname"

## SSH

if [ ! -d ~/.ssh ]; then
    mkdir -p ~/.ssh
fi

if test "700" != $(stat -c %a ~/.ssh); then
    info "Setting permissions for ~/.ssh"
    chmod 700 ~/.ssh
fi

if [ ! -f ~/.ssh/id_rsa ]; then
  if yesno "Set up ~/.ssh/id_rsa?"; then
    echo Paste in your id_rsa and them press ^D
    mkdir -p ~/.ssh
    cat > ~/.ssh/id_rsa
    echo Thank you.
    echo
  fi
fi

if test "600" != $(stat -c %a ~/.ssh/id_rsa); then
    info "Setting permissions for ~/.ssh/id_rsa"
    chmod 600 ~/.ssh/id_rsa
fi

if [ ! -f ~/.ssh/id_rsa.pub ]; then
  if yesno "Set up ~/.ssh/id_rsa.pub?"; then
    echo Paste in your id_rsa.pub and them press ^D
    mkdir -p ~/.ssh
    cat > ~/.ssh/id_rsa.pub
    echo Thank you.
    echo
  fi
fi

if test "600" != $(stat -c %a ~/.ssh/id_rsa.pub); then
    info "Setting permissions for ~/.ssh/id_rsa.pub"
    chmod 600 ~/.ssh/id_rsa.pub
fi

# Applications

not_installed emacs && install_application emacs25
not_installed tmux && install_application tmux
not_installed jq && install_application jq
not_installed ag && install_application silversearcher-ag
not_installed docker && install_docker
not_installed ruby && install_rbenv
not_installed node && install_nvm
not_installed go && install_go
not_installed hugo && install_hugo
not_installed heroku && install_heroku
not_installed terraform && install_terraform
not_installed packer && install_packer
not_installed ansible && install_ansible
not_installed atom && install_atom
not_installed gcloud && install_gcloud
not_installed steam && install_steam

# Get workspace

if [ "${USER}" == "wschenk" ]; then
    info Installng local stuff
    if [ ! -d ~/willschenk.com ]; then
	info Cloning my website
	git clone git@github.com:wschenk/willschenk.com.git
    fi


    if [[ ":$PATH:" == *":$HOME/willschenk.com/bin:"* ]]; then
	echo "Your path is correctly set"
    else
	info Adding willschenk.com/bin to the path
	echo 'export PATH=$HOME/willschenk.com/bin:$PATH' >> ~/.profile
	source ~/.profile
	RESET_TERMINAL=1
    fi
fi
