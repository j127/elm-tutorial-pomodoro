function notifyMe(msg) {
    if (!('Notification' in window)) {
        alert('This browser does not support desktop notification');
    }

    else if (Notification.permission === 'granted') {
        var notification = new Notification(msg);
    }

    else if (Notification.permission !== 'denied') {
        Notification.requestPermission(function (permission) {
            // If the user accepts, let's create a notification
            if (permission === 'granted') {
                var notification = new Notification('Hello from the timer!');
            }
        });
    }
}
