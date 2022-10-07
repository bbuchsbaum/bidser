
conf <- "
  1001:
    - func
    - anat
  1002:
    - func
    - anat
  1003:
    - func
    - anat
"

bids_skeleton <- function(project_name, config) {
  skel <- try(yaml::yaml.load_file(config))
  if (inherits(skel, "try-error")) {
    stop(paste("error loading config", config))
  }
  
  sids <- names(skel)
  part_df <- data.frame(participant_id=sids)
  name <- project_name
  
  structure(list(
    name=project_name,
    tree=skel,
    part_df=part_df),
    class="bids_skeleton")
    
}