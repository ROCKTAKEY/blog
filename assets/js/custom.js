function mySetTheme(style){
    document.documentElement.setAttribute('data-theme', style);
    localStorage.setItem('my-theme', style);
}

function customInit() {
    let theme = localStorage.getItem('my-theme') || "dark";
    document.documentElement.setAttribute('data-theme', theme);
    localStorage.setItem('my-theme', theme);
}

function mySwitchTheme(_) {
    var theme = getTheme();
    localStorage.setItem('my-theme', theme);
}

document.addEventListener('DOMContentLoaded', function() {
    var themeSwitcher = document.querySelector('.theme-switch');
    themeSwitcher.addEventListener('click', mySwitchTheme, false);
}, false);

customInit();
