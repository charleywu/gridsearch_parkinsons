# Gridsearch task
Behavioral experiment that can be run locally in a browser. Based on code from several publications, including: 

- Giron, A. P., Ciranka, S., Schulz, E., van den Bos, W., Ruggeri, A., Meder, B., & Wu, C. M. (2023). Developmental changes in exploration resemble stochastic optimization. *Nature Human Behavior, 7*, 1955–1967.

- Meder, B., Wu, C. M., Schulz, E., & Ruggeri, A. (2021). Development of directed and random exploration in children. *Developmental Science, 24*(4), e13095.

- Wu, C. M., Schulz, E., Garvert, M. M., Meder, B., & Schuck, N. W. (2020). Similarities and differences in spatial and non-spatial cognitive maps. *PLOS Computational Biology, 16*, e1008149.

- Schulz, E., Wu, C. M., Ruggeri, A., & Meder, B. (2019). Searching for rewards like a child means less generalization and more directed exploration. *Psychological Science, 30*(11), 1561–1572.

- Wu, C. M., Schulz, E., Speekenbrink, M., Nelson, J. D., & Meder, B. (2018). Generalization guides human exploration in vast decision spaces. *Nature Human Behaviour, 2*, 915–924.

## Running the experiment
Open `ìndex.html`in a browser. Enter an ID. At the end of the session, a `.json` file can be saved. 

## Experimental parameters
Various parameters of the task can be set by changing variables in `js/gridSearch.js`. 

For instance, to change the number of rounds (includes practice and bonus round) you can change the following variable in `js/gridSearch.js`:

```js
totalTrialsNumber = 6, // includes the instructions trial and the bonus trial
```

*Note: there are some hardcoded references to the number of rounds in the text. which need to be set manually.*

To change the amount of spatial correlation in the environment, three pools of 40 environments each exist (`kernelSmooth.json`, `kernelRough.json`, and `kernelMed.json`). The environments differ in the smoothness of the reward function (level of spatial correlation):  `rough` ($\lambda=1$), `medium` ($\lambda=2$), and `smooth` ($\lambda=4$), where $\lambda$ denotes the length-scale of the RBF kernel used to generate the bivariate rewards functions over the grid. Higher values correspond to smoother functions. 
The used type of environment can be set in `js/gridSearch.js` (currently hardcoded to `rough`; can alternatively be set through parameter `condition`in URL and appropriate IF condition):

```js
 // Get condition from URL
    condition = isrcUtils.GetUriParam('condition') ? isrcUtils.GetUriParam('condition') : 0;

    //condition=Math.round(Math.random())
    clicks = horizon; //set initial number of clicks to horizon
    //var kernelFiles = ['kernelSmooth.json', 'kernelRough.json', 'kernelMed.json'];
    //Load initial environments
    if (condition == 0) { //initial env is smooth
      var initialEnvs = roughKernel; // variable from json file
      console.log('[debug] condition: rough');
    } else {
      var initialEnvs = roughKernel; // variable from json file
      console.log('[debug] condition: rough');
    }
}
```

```