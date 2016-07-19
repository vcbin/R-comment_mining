#!bin/bash
# read the options
TEMP=`getopt -o Vhn --long verbose,help,dry-run,amend -n "$0" -- "$@"`
eval set -- "$TEMP"

d_help="n"
verbose="n"
dry_run_opt=""
amend_opt=""
# extract options and their arguments into variables.
while true ; do
        case "$1" in
                -V|--verbose)
                        verbose="y";  shift ;;
                -h|--help)
                        d_help="y";  shift ;;
                -n|--dry-run)
                        dry_run_opt="--dry-run";  shift ;;
                --amend)
                        amend_opt="--amend";  shift ;;
                --) shift ; break ;;
        *) echo "Internal error!" ; exit 1 ;;
esac
done
[[ $# -eq 0 || $d_help == 'y' ]] && echo -e "usage:\t$0\t'commmit message'\t[branch_name]"

cwd=$( dirname "$0" )
cd $cwd
#pwd
[[ ! -d .git ]] && { echo "NOT a git repo directory!!";exit 1; } 
doxy_f_lst=$( find -name 'Doxyfile' 2>/dev/null )
if [[ $doxy_f_lst ]]; then
	doxy_path=$(dirname $doxy_f_lst)
	cd $doxy_path && doxygen
	cd $cwd
fi
commit_dir="commit_info"
commit_out_f="${commit_dir}/commit_out"
[[ ! -d $commit_dir ]] && mkdir -p $commit_dir
echo
git commit -a -m"$1" $amend_opt $dry_run_opt > "$commit_out_f"
head "$commit_out_f"
echo
echo -e "git commit output '$commit_out_f'"
if [[ -z $dry_run_opt ]]; then
        sha=$( git rev-parse HEAD | head -n 1 )
        status_f="${commit_dir}/${sha}_status"
        flist_f="${commit_dir}/${sha}_files"
        git status > "$status_f"
        echo 
        echo -e "current git commit status output '$status_f'"
        git ls-tree --name-only -r HEAD > "$flist_f"
        echo 
        echo -e "current git commit file list '$flist_f'"
        if [[ $verbose == 'y' ]]; then
                echo 
                head "$status_f"
                echo 
                head "$flist_f"
        fi
fi
cur_branch_n=$(git rev-parse --abbrev-ref HEAD)
branch_n=${2:-${cur_branch_n}}
pull_out=$( git pull origin $branch_n )
echo
echo -e "$pull_out"
ret_code=$?
[[ $ret_code -ne 0 ]] && { echo -e "pull return with FAILURE: ${ret_code}"; exit 1; }
push_out=$( git push origin $branch_n $dry_run_opt )
echo
echo -e "$push_out"
ret_code=$?
[[ $ret_code -ne 0 ]] && { echo -e "push return with FAILURE: ${ret_code}"; exit 2; }
