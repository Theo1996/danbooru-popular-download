#popular rstudio 	w8VJxfgr4ixcdaj9nyKkDZiu
setwd("E:\\My Documents\\Danbooru R")
api.key="w8VJxfgr4ixcdaj9nyKkDZiu"
load("~/Danbooru R/danbooru.RData")
library(jsonlite)
library(httr)
library(tools)
blacklist=c("overwatch","gawr_gura","kevcrexx","coro_fae","dokibird_(vtuber)","kson","doki_doki_literature_club","rwby","onepiece","kiryu_coco","mavixtious","zuharu")
#library(httr2)
#library(rvest)
#library(curl)
#date="2024-07-31" #date=c()
if (exists("ds")==FALSE) ds=as.Date("2024-07-31","%Y-%m-%d") #auto advance date if it dls 200 pics,add check  for errors so it doesnt increment withouy reason
if (exists("nd")==FALSE) nd=1
if (exists("date1")==FALSE) date1=c(ds[1] + nd)
date1[nd]=c(ds[1] + nd)
res1=GET(url=paste0("https://danbooru.donmai.us/explore/posts/popular.json?date=",date1[nd],"&scale=day&limit=200&login=theo1996&api_key=",api.key))
if (status_code(res1) != 200) {
  stop("Failed to fetch data from Danbooru API")
}
x2=(content(res1 ,as = "parsed"))
#https://danbooru.donmai.us/explore/posts/popular.json?date=2024-05-09&scale=day&limit=200
#.json?tags=order%3Ascore%20date%3A2024-05-09&limit=200
dir=paste0("E:\\Downloads\\danbooru\\",date1[nd],"\\")
if (!dir.exists(dir)) {
  dir.create(dir)
}

sink(paste0(dir,"log.txt"),append = TRUE)

e=length(dir(dir))-1 # restart DL where we left off
#e=198
if (e<2) e=0
for ( i in seq(1+e,length(x2))) {
  cat("No.",i,"\n")
  #print(c("https://danbooru.donmai.us/posts/",x2[[i]][["id"]]),sep="")
  cat(c("https://danbooru.donmai.us/posts/",x2[[i]][["id"]]),"\n",sep="")
 # cat(paste0("https://danbooru.donmai.us/posts/",x2[[i]][["id"]]),"\n")
  tags = strsplit(x2[[i]][["tag_string"]]," ")[[1]]
  bm = tags %in% blacklist
  bma = 0
  for (ib in seq(1:length(bm))) {
    if (bm[ib]==TRUE){bma=1
      #print(c(bm[ib],tags[ib],"\n"))
      cat(c(bm[ib],tags[ib],"\n"))
      break
      }
  }
 # seq_along(bm)
  #seq_
 
  if ( length(x2[[i]][["media_asset"]][["variants"]])!=0 & bma==0 ){#if loli gold memb or blackilisted
    for ( b in 1:length(x2[[i]][["media_asset"]][["variants"]]) ) 
      f=1
      iter=0
      if ( x2[[i]][["media_asset"]][["file_ext"]] != "zip") {#if not video
        if (x2[[i]][["media_asset"]][["variants"]][[b]][["type"]] == "original") {
          cat(b)
          c=b
           while (f!=0 & iter<5) { # f=0(success) repeat if error
              tryCatch( {#no immediate interupt if error
          f=download.file( url =
                           x2[[i]][["media_asset"]][["variants"]][[b]][["url"]],destfile =
                           paste0(dir,
                                  gsub("[^a-zA-Z0-9._-]", "_",
                                       substr(paste0("__",x2[[i]][["tag_string_character"]],
                                                     "_",x2[[i]][["tag_string_copyright"]],
                                                     "_drawn_by_",x2[[i]][["tag_string_artist"]],
                                                     "__"),1,170)),x2[[i]][["md5"]],".",
                                  x2[[i]][["file_ext"]]
                           ) , method = "auto" ,quiet = TRUE ,mode = "wb",cacheOK = FALSE)},
          error =function(e)  {
                Sys.sleep(8)
                iter=iter+1
              
                if (iter>=5) {stop("Failed to DL 5 times, check internet connection.")}else{cat(c("retrying",iter,x2[[i]][["id"]]))}
          })
           }
        }
      } else if (x2[[i]][["media_asset"]][["variants"]][[b]][["type"]] == "sample") { #if video 
          cat(x2[[i]][["media_asset"]][["variants"]][[b]][["type"]])
          c=b 
          while (f!=0 & iter<5) { # f=0(success) repeat if error
          tryCatch( {
           f=download.file( url = x2[[i]][["media_asset"]][["variants"]][[b]][["url"]],destfile =
                           paste0(dir,gsub("[^a-zA-Z0-9._-]", "_",substr(
                             paste0(#w7 can only have 256 chars total
                               "__",x2[[i]][["tag_string_character"]],"_",
                               x2[[i]][["tag_string_copyright"]],"_drawn_by_"
                               ,x2[[i]][["tag_string_artist"]],"__")
                             ,1,170)),x2[[i]][["md5"]],".",
                             x2[[i]][["media_asset"]][["variants"]][[b]][["file_ext"]]) , method = "auto",quiet = FALSE ,mode = "wb",cacheOK = TRUE)},
           error =function(e) {
             Sys.sleep(8)
             iter=iter+1
             if (iter>=5) {stop("Failed to DL 5 times, check internet connection.")}else{cat(c("retrying",iter,x2[[i]][["id"]]))}
               })
        }
      }
    }else if(bma==1){
      cat("blacklisted tag:",tags[ib],"\n")
  }else if ( length(x2[[i]][["media_asset"]][["variants"]])!=0){
    cat("Probably a Gold Membership image only.External link:",x2[[i]][["source"]],"\n")
  }
  if ( bma==0 ){
  cat("Variant:",c)
  cat(x2[[i]][["media_asset"]][["variants"]][[c]][["type"]])
  print(paste0(dir,gsub("[^a-zA-Z0-9._-]", "_",substr(
    paste0(
      "__",x2[[i]][["tag_string_character"]],"_",
      x2[[i]][["tag_string_copyright"]],"_drawn_by_"
      ,x2[[i]][["tag_string_artist"]],"__",x2[[i]][["md5"]])
    ,1,200)),".",
    x2[[i]][["media_asset"]][["variants"]][[c]][["file_ext"]]))
  }
  Sys.sleep(1)
if (i == 200) nd=nd+1
}
sink()
#}


##write(res,"getHtml.json")
##testbooru section
#res = GET(paste0("https://$theo1996:$",api.key,"@testbooru.donmai.us/posts/6.json&limit=200"))
#write(as.character(x2),"getHtml.json")
#rea=read_html(paste0("https://$theo1996:$",api.key,"@testbooru.donmai.us/posts/6.json"))
#
#res=GET(paste0("https://$theo1996:$",api.key,"@testbooru.donmai.us/posts?tags=order%3Ascore%20age%3A%3C1day&limit=200"))
##order and limit 200 compare section AGE<1DAY
#rea=read_html(paste0("https://$theo1996:$",api.key,"@danbooru.donmai.us/posts?tags=order%3Ascore%20age%3A%3C1day&limit=200")) 
#res=GET(paste0("https://$theo1996:$",api.key,"@danbooru.donmai.us/posts?tags=order%3Ascore%20age%3A%3C1day&limit=200"))
#write(as.character(rea),"getHtml.json")
#write(as.character(res),"getJson.json")
##order and limit 200 compare section AGE:<CURRENT DATE>
#rea=read_html(paste0("https://$theo1996:$",api.key,"@danbooru.donmai.us/posts?tags=order%3Ascore%20date%3A2024-05-27&z=5&limit#=200")) 
#res1l=as_list(res1)
#write(res1)
#write(as.character(res1),"res1.json")
#write(as.character(rea),"getHtml.json")
#write(as.character(rea),"getHtml.json")
#write(as.character(res),"getJson.json")
##elemrea=html_elements(rea,"article")
##write(as.character(elemrea),"elemJson.json")
##elemres=html_elements(res,"article") doesnt work because it wasnt read with read_html
##elemreaa=html_elements(elemrea,"a")
##chilrea=html_children(elemrea)
##attrrea=html_attr(elemreaa,"href")
#href=html_elements(rea,"article") %>% html_elements("a") %>% html_attr("href")
#for (i in length(href)) {
#  read_html(paste0("https://$theo1996:$",api.key,"@danbooru.donmai.us",href[1])) %>% html_elements(".image-view-original-link"#) %>% html_attr("href")
#}
#
#write(as.character( read_html(paste0("https://$theo1996:$",api.key,"@danbooru.donmai.us",href[1])) %>% html_elements(".image#-view-original-link")) ,"sectionHtml.json")
#read_html(paste0("https://$theo1996:$",api.key,"@danbooru.donmai.us/posts?",href[])) 
#
##popular limit 200 section
#resPop=read_html(paste0("https://$theo1996:$",api.key,"@danbooru.donmai.us/explore/posts/popular?&limit=200")) 
#write(as.character(resPop),"getHtmlpop.json")
#
#
##res1=curl(url=paste0("https://theo1996:",api.key,"@danbooru.donmai.us/profile.json"),add_headers(Authorization=api.key,login#="theo1996"))
##res1=curl(url=paste0("https://danbooru.donmai.us/profile.json"),open="rb",add_headers(Authorization=api.key,username#="theo1996",api_key=api.key,login="theo1996"))
##res1=GET(url=paste0("https://danbooru.donmai.us/profile.json"),add_headers(Authorization=c("Authentication: Basic $",secret#),username="theo1996",api_key=api.key,login="theo1996"))
##res1=GET(url=paste0("https://danbooru.donmai.us/profile.json?login=theo1996&api_key=",api.key))
#
#  
## if (x2[[i]][["file_ext"]])=="jpg" || x2[[i]][["file_ext"]])=="png" ) {
##    download.file(url=)
##  } else if ( x2[[i]][["file_ext"]] =="mp4" ) {
## download.file(url=)
#  #have to make cases for images,mp4,gifs,webms and zip
#  #x2[[47]][["media_asset"]][["variants"]][[5]][["type"]]
#
#
##for ( post in x2 ){
##  print(post)
##  #print(x2[[i]][["media_asset"]][["variants"]][[5]][["url"]])
##  #print(x2[[i]][["file_ext"]])
##  #have to make cases for images,mp4,gifs,webms and zip
##  #x2[[47]][["media_asset"]][["variants"]][[5]][["type"]]
##}
#
#
#lapply(x2, function(x) x[1]$media_asset.id)
#lapply(x2, `[` , 1:10)
#
##res=read_html(paste0("https://$theo1996:$",api.key,"@danbooru.donmai.us/posts?tags=order%3Ascore%20age%3A%3C1day&limit=200")) #
#"https://$theo1996:$w8VJxfgr4ixcdaj9nyKkDZiu@danbooru.donmai.us/posts?tags=order%3Ascore%20age%3A%3C1day&limit=200"
##order:score date:<currentdate>
##<a class="post-preview-link" draggable="false" href="/posts/7651311">
##https://danbooru.donmai.us/posts?tags=order%3Ascore%20age%3A%3C1day
##post-preview-link
##<img src=
##https://testbooru.donmai.us/posts/6.json
##https://testbooru.donmai.us/
##https://$username:$api_key@danbooru.donmai.us/profile.json
##https://danbooru.donmai.us/explore/posts/popular?date=2024-05-06&scale=day
##https://danbooru.donmai.us/posts?tags=order%3Ascore+date%3A%222024-5-31%22&z=5
##order:score date:"2024-5-31"
#https://danbooru.donmai.us/posts?tags=order%3Ascore+date%3A2024-05-27&z=5
#
#.headers=c(
#           Accept-Encoding= "gzip, deflate, br",
#           Accept-Language= "en-US,en;q=0.5")
#
#x2[[47]][["media_asset"]][["variants"]][[5]][["type"]]
##danboory ruby filename code
#def humanized_essential_tag_string
#chartags = tags_for_category("character")
#characters = chartags.max_by(5, &:post_count).map(&:unqualified_name)
#characters += ["#{chartags.size - 5} more"] if chartags.size > 5
#characters = characters.to_sentence
#
#copytags = tags_for_category("copyright")
#copyrights = copytags.max_by(1, &:post_count).map(&:unqualified_name)
#copyrights += ["#{copytags.size - 1} more"] if copytags.size > 1
#copyrights = copyrights.to_sentence
#copyrights = "(#{copyrights})" if characters.present? && copyrights.present?
#  
#  artists = tags_for_category("artist").map(&:name).grep_v("banned_artist").to_sentence
#artists = "drawn by #{artists}" if artists.present?
#  
#  "#{characters} #{copyrights} #{artists}".strip

#end
#it download but the date and timezone are probably wrong,fixed!!
#sapply(x2[[]][["media_asset"]][["variants"]][[5]][["type"]],`[[`,10)


#print(x2[[i]][["media_asset"]][["variants"]][[5]][["url"]])
#print(x2[[i]][["file_ext"]])
#print(x2[[i]][["media_asset"]][["file_ext"]])
#print(x2[[i]][["media_asset"]][["variants"]][[2]][["type"]] )
#add md5 check to skip dl'd files 
#md5i=i
#for (i in length(md5)) {
#  if (md5[i]==x2[[md5i]][["md5"]]) e=true
#}
#& md5[i]!=x2[[i]][["md5"]]

#list.files(path=dir,pattern=c("jpg","png","mp4","webm","gif"))
#md5list=as.vector(md5sum(dir(dir,full.names = TRUE)))
#for (im in 1:length(md5list)) {
#  for (ix in 1:length(x2) ){
#    print(c(im,ix,md5list[im],x2[[ix]][["md5"]]))
#   if (md5list[im]==x2[[ix]][["md5"]]) int[ix]=1
#  }
#}
#frequency(int[])