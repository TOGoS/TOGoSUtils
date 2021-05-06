# Given a blob-interpreting URN (x-rdf-subject:URN, x-parse-rdf:URN, or URN#),
# return just the blob part.
extract_blob_urn() {
    urn="$1"
    re1='^(x-rdf-subject|x-parse-rdf):(.*)$'
    re2='^(.*)#$'
    if [[ $urn =~ $re1 ]] ; then
	echo "${BASH_REMATCH[2]}"
    elif [[ $urn =~ $re2 ]] ; then
	echo "${BASH_REMATCH[1]}"
    else
	echo "$urn"
    fi
}

# $1 = URN of commit
get_ccouch_commit_target() {
    commit_urn="$1"
    commit_blob_urn="$(extract_blob_urn "$commit_urn")"
    target_line="$(ccouch cat "$commit_blob_urn" | grep '<target rdf:resource=".*"/>')"
    if [ -z "$target_line" ] ; then
	echo "$self_name: error: Couldn't find target of commit $commit_urn" >&2
	exit 1
    fi
    re='<target rdf:resource="(.*)"/>'
    if [[ $target_line =~ $re ]] ; then
	echo "${BASH_REMATCH[1]}"
    else
	echo "$self_name: error: Couldn't find target of commit $commit_urn" >&2
	echo "I couldn't parse this line for a target:" >&2
	echo "  $target_line" >&2
	exit 1
    fi
}
