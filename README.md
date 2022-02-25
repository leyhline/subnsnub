# subnsnub - helps to create subtitles from and for audiobooks

I created this tool for my Japanese studies but generally it is language agnostic. Since I like reading books I wanted to match written sentences with their spoken counterpart from audio books. It is directly tailored to my needs and I may implement more flexibility only if necessary.

Parts of the program depent on [FFmpeg](https://ffmpeg.org/) for detecting silence intervals in audio files and on [Anki-Connect](https://foosoft.net/projects/anki-connect/) for creating flash cards in [Anki](https://apps.ankiweb.net/).

## Usage

You can download a static executable from the [Releases](https://github.com/leyhline/subnsnub/releases) section on GitHub.

There are a number of subcommands for processing different file types. My workflow is as following:

1. `subnsnub xmlextract book.xhtml -o book.txt` to extract paragraphs of text from eBook/HTML.
2. `subnsnub audio2sub book.ogg -o book.srt` to create a subtitle template without text in SRT format.
3. Then I manually edit and fill the `book.srt` file with text from `book.txt`. Currently, I use [Subtitle Composer](https://subtitlecomposer.kde.org/) but it really does not matter. Incidentally, one could partly automate this part (with lots of effort) but since my goal is to learn a language it is more effective to do this by hand.
4. `subnsnub sub2html book.srt book.ogg -o book.html` creates an interactive web page one can open with a browser displaying the clickable book text. It reads the corresponding audio part if the audio file is in the same folder.
5. `subnsnub send2anki book.srt book.ogg` creates Anki flash cards if there is a running Anki instance with Anki-Connect installed. It depends on the AJATT stack from <https://tatsumoto.neocities.org/>. This part is hard coded since I currently have no need for flexibility.

For examples, see the `docs/` folder or <https://leyhline.github.io/subnsnub/>.

## Development

Essentially, you only need the Haskell Tool [Stack](https://docs.haskellstack.org/) for building an executable.

Just building the program is `stack build`, to additionally running tests it is `stack test` and for running it from the current code call `stack run -- --help`.

### Notes

In my opinion the current solution is not optimal but that's okay with me. Originally, I just wanted to get more familiar with Haskell and I succeeded on this front. From a more objective standpoint, one would structure this differently.

1. Instead of statically generating HTML from SRT files it would have been better to write a SRT parser in JavaScript to dynamically generate the HTML document. This wouldn't habe been much harder. This way, one can just upload SRT files and corresponding audio files without running `subnsnub sub2html` in between.
2. Moreover, insted of hardcoding the HTML it would have been more flexible to use some kind of templating and providing the template with the executable. Personally, I wanted a single executable without data or config files but making changes is more painful this way.
3. I am also not sure about calling FFmpeg as subcommand and textually parsing the output. Maybe using the some kind of C API or wrapper would habe been better? Not sure about this.
