pipeline {
  agent {
    kubernetes {
      label 'api0-dev-builder'
      yaml """
apiVersion: v1
kind: Pod
spec:
  containers:
  - name: dev-alpine-erlang
    image: yhuangsh/dev-alpine-erlang:latest
    tty: true
"""
    }
  }
  stages {
    // Pipeline implies SCM clonse/checkout, no explicit git clone needed
    // May consider remove git from the dev image, but decided to leave it for now
    stage('Build & Unit Test') {
      steps {
        // rebar3 had to run within the container because it's not included in the Jenkins image
        container('dev-alpine-erlang') {
          sh 'rebar3 eunit --name app'
          sh 'rebar3 ct --name app'
        }
      }
    }
    stage('Release') {
      steps {
        container('dev-alpine-erlang') {
          sh 'rebar3 release'
        }
      }
    }
    stage('Tag') {
      steps {
        // Credit: https://github.com/jenkinsci/pipeline-examples/blob/master/pipeline-examples/push-git-repo/pushGitRepo.groovy 
        // This keeps the github username/password, or SSH key within the Jenkins configuration, no having to be stored in 
        // the dev container image
        withCredentials([usernamePassword(credentialsId: '76b47592-7939-449d-a880-12ec200fcf84', passwordVariable: 'GIT_PASSWORD', usernameVariable: 'GIT_USERNAME')]) {
          sh 'git tag -a b${BUILD_NUMBER} -m "successful dev build tagged by Jenkins"'
          sh 'git push https://${GIT_USERNAME}:${GIT_PASSWORD}@github.com/yhuangsh/api0 --tags'
        }
      }
    }
    stage('Make & Push Image') {
      environment {
          DOCKER_HOST='tcp://172.17.94.121:2375'
          API0_IMAGE='yhuangsh/api0-dev-build'
          API0_IMAGE_TAG='latest'
      }
      steps {  
        // Would consider install docker client and docker step plugin on Jenkins slave image
        // This way the dev image does not need to have docker client installed and 
        // running docker client will not need to be within the 'container' block, also credential and DOCKER_HOST
        // set up will be easier on the Jenkin's UI. 
        container('dev-alpine-erlang') {
          withCredentials([usernamePassword(credentialsId: 'e930ac8a-26be-46b3-aa47-2a716ec2cf0c', passwordVariable: 'DOCKERIO_PASSWORD', usernameVariable: 'DOCKERIO_USERNAME')]) {
            sh 'docker login -u ${DOCKERIO_USERNAME} -p ${DOCKERIO_PASSWORD}'
            sh 'docker build -f priv/Dockfile.dev-build -t ${API0_IMAGE}:${API0_IMAGE_TAG} .'
            sh 'docker push ${API0_IMAGE}:${API0_IMAGE_TAG}'
            sh 'docker image prune -f'
          }
        }
      }
    }
  }
}