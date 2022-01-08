# subnsnub - helps a person to create subtitles for audio books

Build by using [Stack](https://docs.haskellstack.org/). Depends on [FFmpeg](https://ffmpeg.org/).

## General Idea

1. Use an EPUB or (similar ebook format) and an audio book as input.
2. Extract text paragraphs and audio timings (by detecting silence).
3. A person needs to manually match text and timings.
4. Finally, produce subtitle file (SRT or VTT) and maybe interactive HTML.
