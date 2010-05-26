writedotfile = function(ninfo, einfo, filename, fontsize=10, shape='circle', width=0.1,penwidth=1,fixedsize=TRUE){

  if(missing(filename))
     stop("please provide a filename")
  if (is.vector(ninfo)) ninfo = data.frame(node=ninfo, stringsAsFactors=FALSE)

  else ninfo = as.data.frame(ninfo)
  einfo = as.data.frame(einfo)
  nodes = ninfo[,'node']

  ## nodes info
  ## nodes color, white by default
  if (is.null(ninfo$color)) ninfo = cbind(ninfo, color=NA, stringsAsFactors=FALSE)
  if (is.null(ninfo$label)) ninfo = cbind(ninfo, label=nodes, stringsAsFactors=FALSE)
  ninfo$color[is.na(ninfo$color)] = '#ffffff'
  ninfo$label[is.na(ninfo$label)] = ''

  ## edges info
  if (is.null(einfo$color)) einfo = cbind(einfo, color=NA, stringsAsFactors=FALSE)
  
  ## write prolog
  if(fixedsize==TRUE)
    prolog = sprintf('graph foo {\nnode [shape=%s, style=\"filled,setlinewidth(0.4)\", fillcolor=white, width="%f", fontsize=%f, fixedsize=true];', shape, width, fontsize)
  else
    prolog = sprintf('graph foo {\nnode [shape=%s, style=\"filled,setlinewidth(0.4)\", fillcolor=white, width="%f", fontsize=%f, fixedsize=false];', shape, width, fontsize)
  
  prolog = paste(prolog,'edge [];',sep='\n')
  prolog = paste(prolog,'graph [outputorder=edgesfirst,size="8.1,8.1"];',sep='\n')
  prolog = paste(prolog, 'graph [overlap=prism splines=false];',sep='\n')
  cat(prolog, '\n', file=filename, append=F)

  ## write nodes
  tlab = paste('label="', ninfo$label, '"', sep='')
  tnodes = paste('n', ninfo$node, ' [fillcolor="', ninfo$color,'" ', tlab,'] ;', sep='') 
  tnodes = paste(tnodes ,collapse='\n')
  cat(tnodes, '\n', file=filename, append=T)

  ## write edges
  tedges = paste('n', nodes[einfo[,1]], ' -- n', nodes[einfo[,2]], ' [len=', format(einfo$dist, sci=F, digits=5), ' ', sep='')
  tedgescolor = rep('style="invis"', nrow(einfo))
tedgescolor[!is.na(einfo$color)] = paste('color="', einfo$color[!is.na(einfo$color)] ,'"', sep='')

  tedgepw = paste(' penwidth=',penwidth)
  tedges = paste(tedges, tedgescolor,tedgepw,'] ;', sep='')
  tedges = paste(tedges, collapse='\n')
  cat(tedges,'\n',file=filename,append=T)

  cat('}\n',file=filename,append=T)
}
