# Data Cache Utilities
# =============================================================================
# Caching layer for scenario results

#' Cache manager for scenario data
#'
#' @description
#' Manages cached scenario results to avoid recomputing projections
#' for the same configuration.
CacheManager <- R6::R6Class(

  "CacheManager",

  public = list(
    #' @field cache_dir Directory for cache files
    cache_dir = NULL,

    #' @field max_size Maximum number of cached items
    max_size = 50,

    #' @field cache_index In-memory index of cached items
    cache_index = NULL,

    #' Initialize cache manager
    #' @param cache_dir Directory for cache files
    #' @param max_size Maximum cache entries
    initialize = function(cache_dir = SCENARIOS_DIR, max_size = 50) {
      self$cache_dir <- cache_dir
      self$max_size <- max_size
      self$cache_index <- private$load_index()
    },

    #' Get cached result by config hash
    #' @param config Configuration list
    #' @return Cached result or NULL
    get = function(config) {
      hash <- private$config_hash(config)

      if (hash %in% names(self$cache_index)) {
        entry <- self$cache_index[[hash]]
        file_path <- file.path(self$cache_dir, paste0("cache_", hash, ".rds"))

        if (file.exists(file_path)) {
          # Update access time
          self$cache_index[[hash]]$accessed <- Sys.time()
          private$save_index()

          return(readRDS(file_path))
        }
      }

      NULL
    },

    #' Store result in cache
    #' @param config Configuration list
    #' @param result Projection result
    set = function(config, result) {
      hash <- private$config_hash(config)
      file_path <- file.path(self$cache_dir, paste0("cache_", hash, ".rds"))

      # Save to disk
      saveRDS(result, file_path)

      # Update index
      self$cache_index[[hash]] <- list(
        hash = hash,
        created = Sys.time(),
        accessed = Sys.time(),
        size = file.info(file_path)$size
      )

      # Enforce max size
      private$evict_if_needed()

      private$save_index()
    },

    #' Check if config is cached
    #' @param config Configuration list
    #' @return Logical
    has = function(config) {
      hash <- private$config_hash(config)
      hash %in% names(self$cache_index)
    },

    #' Invalidate cached entry
    #' @param config Configuration list
    invalidate = function(config) {
      hash <- private$config_hash(config)

      if (hash %in% names(self$cache_index)) {
        file_path <- file.path(self$cache_dir, paste0("cache_", hash, ".rds"))
        if (file.exists(file_path)) {
          file.remove(file_path)
        }
        self$cache_index[[hash]] <- NULL
        private$save_index()
      }
    },

    #' Clear entire cache
    clear = function() {
      cache_files <- list.files(
        self$cache_dir,
        pattern = "^cache_.*\\.rds$",
        full.names = TRUE
      )

      file.remove(cache_files)
      self$cache_index <- list()
      private$save_index()
    },

    #' Get cache statistics
    #' @return List with cache stats
    stats = function() {
      cache_files <- list.files(
        self$cache_dir,
        pattern = "^cache_.*\\.rds$",
        full.names = TRUE
      )

      total_size <- sum(file.info(cache_files)$size, na.rm = TRUE)

      list(
        n_entries = length(self$cache_index),
        max_entries = self$max_size,
        total_size_mb = total_size / 1e6,
        cache_dir = self$cache_dir
      )
    }
  ),

  private = list(
    #' Compute config hash
    config_hash = function(config) {
      # Remove non-deterministic fields
      config$metadata$created <- NULL
      config$metadata$modified <- NULL
      digest::digest(config, algo = "md5")
    },

    #' Load cache index from disk
    load_index = function() {
      index_file <- file.path(self$cache_dir, ".cache_index.rds")
      if (file.exists(index_file)) {
        tryCatch(readRDS(index_file), error = function(e) list())
      } else {
        list()
      }
    },

    #' Save cache index to disk
    save_index = function() {
      index_file <- file.path(self$cache_dir, ".cache_index.rds")
      saveRDS(self$cache_index, index_file)
    },

    #' Evict oldest entries if over max size
    evict_if_needed = function() {
      while (length(self$cache_index) > self$max_size) {
        # Find oldest accessed entry
        access_times <- sapply(self$cache_index, function(x) x$accessed)
        oldest <- names(which.min(access_times))

        # Remove it
        file_path <- file.path(self$cache_dir, paste0("cache_", oldest, ".rds"))
        if (file.exists(file_path)) {
          file.remove(file_path)
        }
        self$cache_index[[oldest]] <- NULL
      }
    }
  )
)

#' Create global cache instance
create_cache <- function() {
  CacheManager$new()
}

#' Validate cache against current pipeline state
#' @param cache CacheManager instance
#' @param store_path Path to targets store
#' @return Number of invalidated entries
validate_cache <- function(cache, store_path = file.path(ARTEMIS_ROOT, "_targets")) {
  if (!dir.exists(store_path)) {
    return(0)
  }

  # Get pipeline modification time
  pipeline_meta <- file.path(store_path, "meta/meta")
  if (!file.exists(pipeline_meta)) {
    return(0)
  }

  pipeline_mtime <- file.mtime(pipeline_meta)

  # Invalidate entries older than pipeline
  invalidated <- 0
  for (hash in names(cache$cache_index)) {
    entry <- cache$cache_index[[hash]]
    if (entry$created < pipeline_mtime) {
      cache$invalidate(list())  # Need actual config
      invalidated <- invalidated + 1
    }
  }

  invalidated
}
