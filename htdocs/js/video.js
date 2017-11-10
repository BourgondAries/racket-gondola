document.getElementById('video').addEventListener('ended', ended, false);
function ended(handle) {
    if (play_random) {
      history.pushState({
        prevUrl: window.location.href
      }, 'Next page', random_url);
    } else {
      history.pushState({
        prevUrl: window.location.href
      }, 'Next page', next_url + '?v=' + next);
    }
    history.go();
}

function toggle_pause() {
    if (document.getElementById('video').paused) {
       document.getElementById('video').play();
    } else {
       document.getElementById('video').pause();
    }
}

var showing = false;
function showcomment(handle) {
    if (showing) {
      document.getElementById('disqus_thread').style.display = 'none';
    } else {
      if (!loaded_disqus) {
        load_disqus();
        loaded_disqus = true;
      }
      document.getElementById('disqus_thread').style.display = 'block';
    }
    showing = !showing;
}
