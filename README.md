
## Friendly Universe

<img src="friendlyUniverse.png" alt="Friendly Universe" width="450"/>


Friendly Universe is a Qualtrics survey template to anonymously measure the structure of individuals' social networks. Friendly Universe measures networks consisting of nodes, edges, and the goup and closeness of the nodes. This tool is easy to use and can be custromized through Qualtrics and Javascript. 

### To use

Download the Qualtrics qsf file ("friendlyUniverse.qsf"), and import it to your Qualtrics account. 

### Task flow

Below is an demonstration of how this task works.

#### Step 1. Enter nodes

In this section, the task prompts participants to enter the names of their friends/family from different parts of their lives. 


<img src="enterNodes.png" alt="Friendly Universe" width="450"/>


#### Step 2. Delete duplicates

In this section, the task prompts participants to go through their friends one by one and delete any duplicated nodes.


<img src="duplicates.png" alt="Friendly Universe" width="450"/>


#### Step 3. Closeness

In this section, the task prompts participants to indicate how emotionally close they are with each person.


<img src="closeness.png" alt="Friendly Universe" width="450"/>


#### Step 4. Friends of friends

In this section, the task prompts participants to indicate if people in their social network know eacher. 


<img src="fof.png" alt="Friendly Universe" width="450"/>

### Data structure

Data from this task will be stored in Qualtrics as [embedded data](https://www.qualtrics.com/support/survey-platform/survey-module/survey-flow/standard-elements/embedded-data/)

<img src="data.png" alt="Friendly Universe" width="500"/>

* nodes: indicate the closeness level of each node. 1 - extremely close; 2 - very close; 3 - close; 4 - somewhat close; 5 - not very close; 6 - not close; 7 - not at all close.

* edges: indicate whether nodes are connected to each other. Please note that the edges should be __bidirectional__ even the notation suggests single direction (from .. to).

* groups: indicate which groups each node belongs to. 1 - family; 2 - friends; 3 - call; 4 - text; 5 - face to face; 6 - social media.  

Other data (i.e. node labels) are anonymized and not recored. 

### Try it yourself

Try Friendly Universe at this link:

https://upenn.co1.qualtrics.com/jfe/form/SV_0x3BOYw3ScZzzcF

### Contributor

Friendly Universe is based on a previous version of the task, Friendly Ocean, created by Matt B. O'Donnell, Emily B. Falk. 

Rui Pei, Connor Brennan contributed to creating Friendly Universe in Qualtrics. 

Members of the Communication Neuroscience lab has provided generous feedback on this task. 

Etienne Jacquot and Annenberg IT provided IT support. 

### License

Friendly Universe is licensed under [The MIT license](http://opensource.org/licenses/MIT).


