#!/bin/sh

DEFAULT_DIR="${HOME}/sites"
NAME=${1}

SCRIPTS_DIR="${HOME}/.emacs.d/bin/wiki"
WIKI_DIR="${DEFAULT_DIR}/${NAME}"

if [ ! -d ${DEFAULT_DIR} ]; then
    echo "== WARNING =="
    echo "${DEFAULT_DIR} doesn't exists"
    echo "Creating ${DEFAULT_DIR}..."
    mkdir -p ${DEFAULT_DIR}
fi

if [ -z ${NAME} ]; then
    echo "== ERROR =="
    echo "Provide a NAME for the new wiki"
    exit 1
fi

echo "Creating new Wiki site on ${WIKI_DIR}"

mkdir -p ${WIKI_DIR}
mkdir -p ${WIKI_DIR}/content
touch ${WIKI_DIR}/content/index.org

cp ${SCRIPTS_DIR}/* ${WIKI_DIR}
chmod +x ${WIKI_DIR}/build.sh

echo "${NAME} complete!"
