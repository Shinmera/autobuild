#!/bin/bash
readonly QUICKLISP_URL="https://beta.quicklisp.org/quicklisp.lisp"
readonly SHIRAKUMO_URL="http://dist.tymoon.eu/shirakumo.txt"
readonly DEFAULT_TARGET=~/autobuild
readonly DEFAULT_DOMAINS="radiance localhost"

readonly QUICKLISP_FILE="${target}/quicklisp.lisp"
readonly QUICKLISP_DIR="${target}/quicklisp/"
readonly AUTOBUILD_DIR="${target}/projects/"
readonly CONFIG_DIR="${target}/config/"
readonly STARTUP_FILE="${target}/autobuild"

function sbcl-eval(){
    sbcl --noinform \
         --disable-ldb \
         --lose-on-corruption \
         --end-runtime-options \
         --no-userinit \
         --no-sysinit \
         --noprint \
         --load "${QUICKLISP_DIR}setup.lisp" \
         --eval "(progn $@)" \
         --quit
}

function configure(){
    read -p "Enter the installation target [${DEFAULT_TARGET}] " target
    target=${target:-"${DEFAULT_TARGET}"}

    read -p "Enter the list of domains [${DEFAULT_DOMAINS}]" domains
    domains=${domains:-"${DEFAULT_DOMAINS}"}
}

function install-quicklisp(){
    mkdir -p "${QUICKLISP_DIR}"
    mkdir -p "${AUTOBUILD_DIR}"
    mkdir -p "${CONFIG_DIR}"
    
    curl -o "${QUICKLISP_FILE}" "${QUICKLISP_URL}"
    sbcl --no-userinit \
         --load "$QUICKLISP_FILE" \
         --eval "(quicklisp-quickstart:install :path \"${QUICKLISP_DIR}\")" \
         --eval "(ql-dist:install-dist \"${SHIRAKUMO_URL}\" :prompt NIL)" \
         --eval "(sb-ext:quit)"
}

function write-radiance-config(){
        cat >"$1" <<EOF
((:interfaces
  (:logger . "i-verbose")
  (:data-model . "r-simple-model")
  (:database . "i-lambdalite")
  (:user . "r-simple-users")
  (:auth . "r-simple-auth")
  (:session . "r-simple-sessions")
  (:server . "i-hunchentoot"))
 (:server
  (:domains . ("radiance" "localhost" "127.0.0.1" "::1"))
  (:instances . (((:port . 2015)))))
 (:startup :r-simple-errors :autobuild-server))
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
    sbcl-eval "(ql-dist:install-dist \"${SHIRAKUMO_URL}\" :prompt NIL)" \
              "(ql:quickload :radiance)" \
              "(ql:quickload :autobuild-server)"
    
    write-radiance-config "${CONFIG_DIR}/default/radiance-core/radiance.core.conf.lisp"
    write-radiance-launcher "${STARTUP_FILE}"

    (exec "${STARTUP_FILE}" \
          --eval "(radiance:shutdown)" \
          --eval "(sb-ext:quit)")
}

function main(){
    configure
    install-quicklisp
    install-autobuild

    echo ""
    echo "Autobuild installed in ${target}."
    echo "Run ${STARTUP_FILE} to launch autobuild."
}
