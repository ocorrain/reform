function rate(numeric_id,rating) {
    ajax_rate_comment_set_element("comment-" + numeric_id , numeric_id , rating);
}

function reform_toggle(name,type,id) {
    ajax_toggle_tag_set_element("tag-cloud",name,type,id);
    return false;
}

function toggle_visible(obj) {
	var el = document.getElementById(obj);
	el.style.display = (el.style.display != 'none' ? 'none' : '' );
}

function toggle_login_forms() {
    toggle_visible('login-form');
    toggle_visible('registration-form');
    return false;
}

function toggle_contact_form() {
    toggle_visible('contacts-form');
    toggle_visible('new-contact');
    return false;
}