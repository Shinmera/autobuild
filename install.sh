#!/bin/bash
readonly QUICKLISP_URL="https://beta.quicklisp.org/quicklisp.lisp"
readonly SHIRAKUMO_URL="http://dist.tymoon.eu/shirakumo.txt"
readonly DEFAULT_TARGET=~/autobuild
readonly DEFAULT_DOMAINS="radiance localhost"
readonly DEFAULT_PORT=2015

function sbcl-eval(){
    sbcl --noinform \
         --disable-ldb \
         --lose-on-corruption \
         --end-runtime-options \
         --no-userinit \
         --no-sysinit \
         --noprint \
         --load "${QUICKLISP_DIR}setup.lisp" \
         --eval "(progn $*)" \
         --quit
}

function eexit(){
    echo "$(tput setaf 1) ! Error: $(tput sgr 0)" $@
    rm -r "${target:-/dev/null}"
    exit 1
}

function status() {
    local level=$1
    case "$level" in
        0) echo "$(tput setaf 2) ==> $(tput sgr 0)" ${@:2};;
        1) echo "$(tput setaf 3)   -> $(tput sgr 0)" ${@:2};;
        *) echo "$(tput setaf 6)     > $(tput sgr 0)" ${@:2};;
    esac
}

function configure(){
    status 1 "Configuring setup"
    read -p "Enter the installation directory [${DEFAULT_TARGET}] " target
    target=${target:-"${DEFAULT_TARGET}"}

    read -p "Enter the list of domains for this machine [${DEFAULT_DOMAINS}] " domains
    domains=${domains:-"${DEFAULT_DOMAINS}"}

    read -p "Enter the port Autobuild should run on [${DEFAULT_PORT}] " port
    port=${port:-"${DEFAULT_PORT}"}

    readonly QUICKLISP_FILE="${target}/quicklisp.lisp"
    readonly QUICKLISP_DIR="${target}/quicklisp/"
    readonly AUTOBUILD_DIR="${target}/projects/"
    readonly CONFIG_DIR="${target}/config/"
    readonly STARTUP_FILE="${target}/autobuild"
    readonly LISP_PORT=$port
    LISP_DOMAINS=""
    for domain in $domains; do
        LISP_DOMAINS="$LISP_DOMAINS \"$domain\""
    done
}

function install-quicklisp(){
    status 1 "Installing Quicklisp"
    mkdir -p "${QUICKLISP_DIR}"
    mkdir -p "${AUTOBUILD_DIR}"
    
    curl -o "${QUICKLISP_FILE}" "${QUICKLISP_URL}" \
        || eexit "Failed to download Quicklisp file."
    sbcl --no-userinit \
         --load "$QUICKLISP_FILE" \
         --eval "(quicklisp-quickstart:install :path \"${QUICKLISP_DIR}\")" \
         --eval "(ql-dist:install-dist \"${SHIRAKUMO_URL}\" :prompt NIL)" \
         --eval "(sb-ext:quit)" \
        || eexit "Failed to install Quicklisp"
}

function write-radiance-config(){
    mkdir -p $(dirname "$1")
    cat >"$1" <<EOF
((:interfaces
  (:logger . "i-verbose")
  (:data-model . "r-simple-model")
  (:database . "i-lambdalite")
  (:user . "r-simple-users")
  (:auth . "r-simple-auth")
  (:session . "r-simple-sessions")
  (:server . "i-hunchentoot"))
 (:domains . ($LISP_DOMAINS))
 (:startup :r-simple-errors :autobuild-server))
EOF
}

function write-hunchentoot-config(){
    mkdir -p $(dirname "$1")
    cat >"$1" <<EOF
((:default (:port ${LISP_PORT} :address "0.0.0.0")))
EOF
}

function write-radiance-launcher(){
    cat >"$1" <<EOF
#!/bin/sh
sbcl --no-userinit \
     --no-sysinit \
     --load "${QUICKLISP_DIR}setup.lisp" \
     --eval "(ql:quickload '(autobuild radiance))" \
     --eval "(setf autobuild:*base-project-dir* #p\"${AUTOBUILD_DIR}\")" \
     --eval "(setf radiance:*environment-root* #p\"${CONFIG_DIR}\")" \
     --eval "(radiance:startup)" "\$@"
EOF

    chmod +x "$1"
}

function install-autobuild(){
    status 1 "Installing Autobuild"
    write-radiance-config "${CONFIG_DIR}/default/radiance-core/radiance-core.conf.lisp"
    write-hunchentoot-config "${CONFIG_DIR}/default/i-hunchentoot/i-hunchentoot.conf.lisp"
    write-radiance-launcher "${STARTUP_FILE}"

    (exec "${STARTUP_FILE}" \
          --eval "(radiance:shutdown)" \
          --eval "(sb-ext:quit)") \
        || eexit "Failed to setup autobuild"
}

function main(){
    status 0 "Autobuild installer"
    
    configure
    install-quicklisp
    install-autobuild

    echo ""
    status 2 "Autobuild installed in ${target}."
    status 2 "Run ${STARTUP_FILE} to launch autobuild."
}

main
