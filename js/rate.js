function rate(numeric_id,rating) {
    ajax_rate_comment_set_element("comment-" + numeric_id , numeric_id , rating);
}

function reform_toggle(name,type,id) {
    ajax_toggle_tag_set_element("tag-cloud",name,type,id);
}

function toggle_visible(obj) {
	var el = document.getElementById(obj);
	el.style.display = (el.style.display != 'none' ? 'none' : '' );
}