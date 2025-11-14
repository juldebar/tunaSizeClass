# tunaSizeClass


## Running with Docker (recommended)

### Pull and Run

Pulling the image and running it

``` sh

docker pull ghcr.io/firms-gta/tunasizeclass:latest
docker run -p 3838:3838 -v ghcr.io/firms-gta/tunasizeclass:latest 
```

#### Build and Run Locally

If you have cloned the github repository in local you can then:

``` sh
cd the_repo_where_you_pulled_the_shiny_app
docker build -t my-shiny-app .
docker run -p 3838:3838 -v my-shiny-app
```

Access the app by navigating to <http://localhost:3838>.
