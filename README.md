Some links for FFMPEG:  
https://stackoverflow.com/questions/24961127/how-to-create-a-video-from-images-with-ffmpeg  
https://askubuntu.com/questions/648603/how-to-create-an-animated-gif-from-mp4-video-via-command-line/837574#837574  
https://www.reddit.com/r/VideoEditing/comments/yxnco2/is_there_a_way_to_convert_an_image_sequence_into/
  
The command to concat the images:  
```ffmpeg -framerate 60 -pattern_type sequence -i frame%01d.png -s:v 1920x1080 -c:v libx264 -pix_fmt yuv420p out.mp4```