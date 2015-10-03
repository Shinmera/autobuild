#!/bin/bash
readonly QUICKLISP_URL="https://beta.quicklisp.org/quicklisp.lisp"
readonly SHIRAKUMO_URL="http://dist.tymoon.eu/shirakumo.txt"
readonly DEFAULT_TARGET=~/autobuild

read -p "Enter the installation target [${DEFAULT_TARGET}] " target
target=${target:-"${DEFAULT_TARGET}"}

readonly QUICKLISP_FILE="${target}/quicklisp.lisp"
readonly QUICKLISP_DIR="${target}/quicklisp/"
readonly AUTOBUILD_DIR="${target}/projects/"
readonly STARTUP_FILE="${target}/autobuild"

mkdir -p "${QUICKLISP_DIR}"
mkdir -p "${AUTOBUILD_DIR}"

curl -o "${QUICKLISP_FILE}" "${QUICKLISP_URL}"
sbcl --no-userinit \
     --load "$QUICKLISP_FILE" \
     --eval "(quicklisp-quickstart:install :path \"${QUICKLISP_DIR}\")" \
     --eval "(ql-dist:install-dist \"${SHIRAKUMO_URL}\" :prompt NIL)" \
     --eval "(sb-ext:quit)"

cat >"${STARTUP_FILE}" <<EOF
#!/bin/bash

sbcl --no-userinit \
     --load "${QUICKLISP_DIR}setup.lisp" \
     --eval "(ql:quickload :autobuild-server)" \
     --eval "(setf autobuild:*base-project-dir* #p\"${AUTOBUILD_DIR}\")" \
     --eval "(radiance:startup :autobuild)" "\$@"
EOF

chmod +x "${STARTUP_FILE}"

(exec "${STARTUP_FILE}" \
      --eval "(radiance:shutdown)" \
      --eval "(sb-ext:quit)")

echo ""
echo "Autobuild installed in ${target}."
echo "Run ${STARTUP_FILE} to launch autobuild."
