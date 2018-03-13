pipeline {
  agent any
  stages {
    stage('NoAutoMerge') {
      steps {
        timeout(unit: 'MINUTES', time: 2) {
          catchError() {
            svn(url: 'https://62.144.112.9/svn/CaratWaWi/branches/Versionen/W9918.2278.0.0.TestMerge2.0', changelog: true)
          }
          
        }
        
        bat(script: 'set', returnStdout: true)
      }
    }
  }
}