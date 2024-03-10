#!/usr/bin/env sh

# Set defaults.
quick=""
while :; do
    case $1 in
        -h|-\?|--help)
            printf "test.sh "
            printf "[-h|--help]"
            printf "[-Q|--quick]\n"
            exit 0
            ;;
        -Q|--quick)
            quick="--quick"
            ;;
        --)        # End of all options.
            shift
            break
            ;;
        -?*)
            printf 'WARN: Unknown option (ignored): %s\n' "$1" >&2
            ;;
        *)        # Default case.
            break
    esac
    shift
done

emacs ${quick} -nw --eval="(load-theme 'tango-dark)" --load=glsl-db.el --load=glsl-mode.el --load=glsl-ts-mode.el ${1}
