# NEMACS - Emacs Documentation

## Philosophy

It's been more than 2 years since I started to use Emacs for everything. My first commit was on May 2017 and surprisingly I was on MacOS. Although that wasn't my first time using Emacs, as I had use it for work weeks before that on Ubuntu, but it was what pulled me over to the Emacs world as I really hated the Mac and all the environment and Emacs helped me use it a little bit more.

From there I never looked at any other editor, or operating system for that matter. Emacs had it all and it also created some fun moments at work when my friends/co-workers gave me different "challenges" or things to do with Emacs. One of my best friends kind of played with me, but he was using Vim so I had to show him what Emacs could do. First, I showed him Twitter running on Emacs, then I controlled my Spotify playlist with the keyboard and while coding, and after that I had my work chat through ERC and Bitlbee, that was fun.

That same year a lot of things in my life changed. I moved to another Company, started traveling a lot more and learned the ways of Getting Things Done by David Allen, I can say it's one of my favorite books still Today. The only thing that didn't change was Emacs, I still used Emacs for everything. Now I had my Calendar, my Daily Reviews, JIRA tickets and stories, tracking my time, e-mail, chat and obviously, coding. The only problem I found with Emacs is that my configuration and code was getting out of hand, and I couldn't find a way to make it neat while working a lot.

Then 2019 came over and my life is changing again, through the same period as the 2017 changes. Now this inspired me a lot, because I went from a 3 to 4 months hiatus into work again and now my work is not about coding but documenting and analyzing technical solutions. So I thought I have to ride this wave of inspiration and change my Emacs too (but no, not leaving Emacs). Now that I've learned a lot throughout these two years and I have the time to sit down and start all over, I can make this configuration neat, organized and scalable as I always wanted.

So here it's, NEMACS (or Nahuel-EMACS) version 2.0 (actually 5, but yeah) with a clean architecture and neat documentation.

<p style="text-align: right">
Nahuel Jesús Sacchetti
https://nsacchetti.com
</p>

## File Architecture

NEMACS follows the principle of "one function does only one thing" (basic functional programming here). Also, "one file does one thing" can be applied. The idea of separating the logic by module is something that I had in my second version of NEMACS, where the code was written inside an _ORG_ file and it was separated by what the code did in the grand scheme of things.

Now NEMACS has its basics separated in different files, `init.el` loads the whole thing in a "lifecycle" fashion and after everything (Emacs core-related) is initialized, the modules are loaded one by one.

|-----------------|--------|-------------------------------------------------------------------------------------------------------------------|
| Name            | Type   | Description                                                                                                       |
|-----------------|--------|-------------------------------------------------------------------------------------------------------------------|
| ~/.emacs.d      | Folder | The main Emacs configuration folder. Also saved in variable =nemacs-emacs-dir=                                    |
| .local          | Folder | Local folder, not shared upstream. Contains all the system's folders like =cache=, =etc= and =packages=           |
| .local/cache    | Folder | Contains cache files, can be removed to reset some temporary configuration. Example: =autosave=, =history=        |
| .local/etc      | Folder | Persisted configuration is saved here. Like =bookmarks=                                                           |
| .local/packages | Folder | Packages downloaded from =MELPA= live here                                                                        |
| modules         | Folder | Modules files live here. The name of the files inside should be =<modulename>.module.el                           |
| elisp           | Folder | Custom functions and packages that I create are here                                                              |
| themes          | Folder | Contains all the themes I use or used in Emacs                                                                    |
| keybindings.el  | File   | File containing configurations for my custom keybindings                                                          |
| hooks.el        | File   | Contains all the hook bindings                                                                                    |
| init.el         | File   | Initialization file                                                                                               |
| packages.el     | File   | Initialize the Package Manager and downloads the necessary packages                                               |
| preload.el      | File   | Runs before the =after-init-hook=. Contains defaults and configurations that are needed before Emacs ends loading |
| theme.el        | File   | Custom theme changes for the current selected theme are declared here                                             |
| vars.el         | File   | Initialize the custom variables needed for the configuration                                                      |
|-----------------|--------|-------------------------------------------------------------------------------------------------------------------|
