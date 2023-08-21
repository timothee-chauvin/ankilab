function htmlToElement(html) {
    let template = document.createElement('template');
    template.innerHTML = html;
    return template.content.firstChild;
}

function toggleShowTagFilterList() {
    let tag_list = document.getElementById('tag-filter-list')
    if (getComputedStyle(tag_list).display === 'none') {
        tag_list.style.display = 'block'
    } else {
        tag_list.style.display = 'none'
    }
}

function toggleMakeDeckDetails() {
    let extra_div = document.getElementById('make-deck-details')
    let title = document.getElementById('title')
    let description = document.getElementById('description')
    if (this.checked) {
        extra_div.style.display = 'block'
        title.setAttribute('required', '')
        description.setAttribute('required', '')
    } else {
        extra_div.style.display = 'none'
        title.removeAttribute('required')
        description.removeAttribute('required')
    }
}

function toggleAddDeckList(toggle_add_deck_list_btn) {
    let note_id = toggle_add_deck_list_btn.getAttribute('data-note-id')
    let deck_list = Array.from(document.getElementsByClassName('deck-add-list'))
        .filter(tag => tag.dataset.noteId == note_id)[0]
    if (getComputedStyle(deck_list).display === 'none') {
        deck_list.style.display = 'block'
    } else {
        deck_list.style.display = 'none'
    }
}

function updateVote(vote_btn) {
    let vote_type = (vote_btn.classList.contains("upvote-btn") ? "up" : "down")
    let other_vote_type = (vote_type === "up" ? "down" : "up")
    let other_btn = (vote_type === "up" ?
                     vote_btn.nextSibling : vote_btn.previousSibling)
    let note_id = vote_btn.dataset.noteId
    fetch('/note-vote', {
        method: 'POST',
        credentials: 'include',
        headers: {
            'Content-Type': 'application/x-www-form-urlencoded',
        },
        body: new URLSearchParams({
            'note-id': note_id,
            'vote-value': (vote_type === "up" ? 1 : -1)
        }).toString()
    })
        .then(response => {
            if (response.ok) {
                return response.json()
            } else if (response.status === 401) {
                alert('Only logged in users can submit votes')
                throw new Error("not logged in")
            } else {
                alert('Unexpected failure while submitting vote: \
server returned status code ' + response.status)
                throw new Error("unexpected server response")
            }},
              error => {
                  alert('Network failure while submitting vote: ' + error)
                  throw new Error("network failure")
              })
        .then(json => {
            vote_btn.lastChild.innerHTML = json[vote_type]
            other_btn.lastChild.innerHTML = json[other_vote_type]
        },
              error => console.log(error.message))
}

function addToDeck(btn) {
    let deck_id = btn.dataset.deckId
    let note_id = btn.dataset.noteId
    fetch('/decks/add-note', {
        method: 'POST',
        credentials: 'include',
        headers: {
            'Content-Type': 'application/x-www-form-urlencoded',
        },
        body: new URLSearchParams({
            'deck-id': deck_id,
            'note-id': note_id,
        }).toString()
    })
        .then(response => {
            if (response.ok) {
                return response.json()
            } else if (response.status === 401) {
                alert('Only logged in users can submit votes')
                throw new Error("not logged in")
            } else {
                alert('Unexpected failure while submitting vote: \
server returned status code ' + response.status)
                throw new Error("unexpected server response")
            }},
              error => {
                  alert('Network failure while trying to add to deck: ' + error)
                  throw new Error("network failure")
              })
        .then(json => {
            let anki_note_element = btn.parentNode.parentNode.parentNode
            anki_note_element.replaceWith(htmlToElement(json["html"]))
            addHookCallbacks()
        },
              error => console.log(error.message))
}

function addHookCallbacks() {
    /* May be called multiple times, as we sometimes change the DOM content */
    let tag_filter_btn = document.getElementById('tag-filter-btn')
    if (tag_filter_btn) {
        tag_filter_btn.addEventListener('click', toggleShowTagFilterList)
    }
    let make_deck_btn = document.getElementById('make-deck')
    if (make_deck_btn) {
        make_deck_btn.addEventListener('change', toggleMakeDeckDetails)
    }
    let toggle_add_deck_list_btns = Array.from(
        document.getElementsByClassName('toggle-add-deck-list-btn'))
    toggle_add_deck_list_btns.forEach(btn => {
        if (!btn.getAttribute('handled')) {
            btn.setAttribute('handled', true)
            btn.addEventListener('click', () => {toggleAddDeckList(btn)})}})
    let add_deck_btns = Array.from(document.getElementsByClassName('add-deck-btn'))
    add_deck_btns.forEach(btn => {
        if (!btn.getAttribute('handled')) {
            btn.setAttribute('handled', true)
            btn.addEventListener('click', () => {addToDeck(btn)})}})
    let vote_btns = Array.from(document.getElementsByClassName('vote-btn'))
    vote_btns.forEach(btn => {
        if (!btn.getAttribute('handled')) {
            btn.setAttribute('handled', true)
            btn.addEventListener('click', () => {updateVote(btn)})}})
}

document.addEventListener('DOMContentLoaded', addHookCallbacks)
