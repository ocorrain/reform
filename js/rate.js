function rate(numeric_id,rating) {
    ajax_rate_comment_set_element("comment-" + numeric_id , numeric_id , rating);
}

function toggle(name,type,id) {
    ajax_toggle_tag_set_element("tag-cloud",name,type,id);
}
