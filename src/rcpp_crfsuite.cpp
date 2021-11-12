#include <Rcpp.h>
#include <crfsuite.hpp>

// [[Rcpp::export]]
Rcpp::List crfsuite_copyright(){
  Rcpp::List out = Rcpp::List::create(Rcpp::Named("version") = CRFSuite::version(), 
                                      Rcpp::Named("copyright") = CRFSUITE_COPYRIGHT) ;
  
  return out;
}

// [[Rcpp::export]]
Rcpp::List crfsuite_model_parameters(const std::string method, const std::string type = "crf1d"){
  CRFSuite::Trainer model;
  bool model_initialised = model.select(method, type);
  if(!model_initialised){
    Rcpp::stop("Combination of method '" + method + "' and type '" + type + "' is not a valid CRF model.");
  }
  std::vector<std::string> args = model.params();
  std::vector<std::string> args_description;
  std::vector<std::string> args_values;
  for(unsigned int i=0; i<args.size(); i++) { 
    args_description.push_back(model.help(args[i]));
    args_values.push_back(model.get(args[i]));
  }
  Rcpp::List out = Rcpp::List::create(Rcpp::Named("method") = method, 
                                      Rcpp::Named("type") = type, 
                                      Rcpp::Named("params") = Rcpp::DataFrame::create(
                                          Rcpp::Named("arg") = args,
                                          Rcpp::Named("arg_default") = args_values,
                                          Rcpp::Named("description") = args_description,
                                          Rcpp::Named("stringsAsFactors") = false));  
  return out;
}

Rcpp::CharacterVector list_names(Rcpp::List x){
  return x.names();
}


// [[Rcpp::export]]
Rcpp::List crfsuite_model_build(const char* file_model, 
                                const std::vector<int> doc_id, 
                                const std::vector<std::string> y, 
                                Rcpp::CharacterMatrix x,
                                Rcpp::NumericMatrix embeddings,
                                Rcpp::List options,
                                const std::string method = "lbfgs", 
                                const std::string type = "crf1d",
                                int trace = 0){
  // Initialise the model and set the model options
  CRFSuite::Trainer model;
  bool model_initialised = model.select(method, type);
  if(!model_initialised){
    Rcpp::stop("Combination of method '" + method + "' and type '" + type + "' is not a valid CRF model.");
  }
  for (int i=0; i<options.size(); i++) {
    std::string option_name = Rcpp::as<std::string>(list_names(options)[i]);
    std::string option_value = Rcpp::as<std::string>(options[i]);
    model.set(option_name, option_value);
  }
  
  // Converting data to CRFsuite format
  if(trace > 0) Rcpp::Rcout << "Started converting input data to CRFsuite format" << '\n';
  int nfeatures = x.ncol();
  int nrows = doc_id.size();
  int group_start = 0;
  int group_end = 0;
  int previous_group = doc_id[0];
  int current_group = doc_id[0];
  for(int rowidx=0; rowidx<nrows + 1; rowidx++) {
    if(rowidx < nrows){
      current_group = doc_id[rowidx]; 
      if(previous_group == current_group){
        continue;
      } 
    }
    // One sequence is all the data belonging to 1 group
    group_end = rowidx - 1;
    int nterms_in_document = group_end - group_start + 1;
    if(trace > 1) Rcpp::Rcout << "Creating CRFsuite data for doc_id " << previous_group << " containing " << nterms_in_document << " items: " << group_start+1 << "-" << group_end+1 << " \n";
    std::vector<std::vector<CRFSuite::Attribute>> termsequenceattributes;
    std::vector<std::string> labels;
    for(int i=group_start; i<=group_end; i++) { 
      std::vector<CRFSuite::Attribute> termfeatures;
      for(int j=0; j<nfeatures; j++) {  
        if(!Rcpp::CharacterVector::is_na(x(i, j))){
          termfeatures.push_back(CRFSuite::Attribute(std::string(x(i, j)), 1.));  
        }
      }
      if(embeddings.nrow() > 0){
        Rcpp::CharacterVector fields = colnames(embeddings);
        for(int j=0; j<(embeddings.ncol()); j++) {  
          std::string field = Rcpp::as<std::string>(fields[j]);
          if(!Rcpp::NumericVector::is_na(embeddings(i, j))){
            termfeatures.push_back(CRFSuite::Attribute(field, embeddings(i, j)));  
          }
        }
      }
      termsequenceattributes.push_back(termfeatures);
      labels.push_back(y[i]);
    }
    model.append(termsequenceattributes, labels, previous_group);
    // The start point of the next group is the current row
    group_start = rowidx;
    previous_group = current_group;
  }
  
  // Build the model
  if(trace > 0) Rcpp::Rcout << "Started building CRFsuite model in file " << file_model << '\n';
  model.train(file_model, -1);
  if(trace > 0) Rcpp::Rcout << "Ended building CRFsuite model" << '\n';
  // Get the labels from the model
  CRFSuite::Tagger modeltagger;
  modeltagger.open(file_model);
  std::vector<std::string> output_labels = modeltagger.labels();
  // Get the model parameters
  std::vector<std::string> args = model.params();
  std::vector<std::string> args_description;
  std::vector<std::string> args_values;
  for(unsigned int i=0; i<args.size(); i++) { 
    args_description.push_back(model.help(args[i]));
    args_values.push_back(model.get(args[i]));
  }

  // Return file + labels
  Rcpp::List out = Rcpp::List::create(Rcpp::Named("method") = method, 
                                      Rcpp::Named("type") = type,
                                      Rcpp::Named("labels") = output_labels,
                                      Rcpp::Named("options") = Rcpp::DataFrame::create(
                                        Rcpp::Named("arg") = args,
                                        Rcpp::Named("arg_value") = args_values,
                                        Rcpp::Named("description") = args_description,
                                        Rcpp::Named("stringsAsFactors") = false),
                                      Rcpp::Named("file_model") = file_model);
  return out;
}


// [[Rcpp::export]]
Rcpp::List crfsuite_model(const char* file_model){
  // Get the labels from the model
  CRFSuite::Tagger modeltagger;
  modeltagger.open(file_model);
  std::vector<std::string> output_labels = modeltagger.labels();

  // Return file + labels
  Rcpp::List out = Rcpp::List::create(Rcpp::Named("labels") = output_labels,
                                      Rcpp::Named("file_model") = file_model);
  return out;
}

// [[Rcpp::export]]
void crfsuite_model_dump(const char* file_model, const char* file_txt){
  FILE * pFile = fopen (file_txt, "w");
  crfsuite_model_t *model = NULL;
  crfsuite_create_instance_from_file(file_model, (void**)&model);
  model->dump(model, pFile);
  model->release(model);
  fclose (pFile);
  return;
}


// [[Rcpp::export]]
Rcpp::List crfsuite_model_coefficients(const char* file_model){
  crfsuite_model_t *model = NULL;
  crfsuite_create_instance_from_file(file_model, (void**)&model);
  SEXP coefficients = model->dump_coefficients(model);
  Rcpp::List out(coefficients);
  model->release(model);
  return out;
}

// [[Rcpp::export]]
Rcpp::List crfsuite_predict(const std::string file_model, 
                            const std::vector<int> doc_id, 
                            Rcpp::CharacterMatrix x,
                            Rcpp::NumericMatrix embeddings,
                            int trace = 0){
  std::vector<std::string> labels;
  std::vector<double> marginal;
  std::vector<int> sequences;
  std::vector<double> sequence_probabilities;
  
  // Load the model from file
  CRFSuite::Tagger model;
  model.open(file_model);
  
  // Converting data to CRFsuite format
  if(trace > 0) Rcpp::Rcout << "Started converting input data to CRFsuite format" << '\n';
  int nfeatures = x.ncol();
  int nrows = doc_id.size();
  int group_start = 0;
  int group_end = 0;
  int previous_group = doc_id[0];
  int current_group = doc_id[0];
  for(int rowidx=0; rowidx<nrows + 1; rowidx++) {
    if(rowidx < nrows){
      current_group = doc_id[rowidx]; 
      if(previous_group == current_group){
        continue;
      } 
    }
    // One sequence is all the data belonging to 1 group
    group_end = rowidx - 1;
    int nterms_in_document = group_end - group_start + 1;
    if(trace > 1) Rcpp::Rcout << "Creating CRFsuite data for doc_id " << previous_group << " containing " << nterms_in_document << " items: " << group_start+1 << "-" << group_end+1 << " \n";
    std::vector<std::vector<CRFSuite::Attribute>> termsequenceattributes;
    for(int i=group_start; i<=group_end; i++) { 
      std::vector<CRFSuite::Attribute> termfeatures;
      for(int j=0; j<nfeatures; j++) {  
        if(!Rcpp::CharacterVector::is_na(x(i, j))){
          termfeatures.push_back(CRFSuite::Attribute(std::string(x(i, j)), 1.));
        }
      }
      if(embeddings.nrow() > 0){
        Rcpp::CharacterVector fields = colnames(embeddings);
        for(int j=0; j<(embeddings.ncol()); j++) {  
          std::string field = Rcpp::as<std::string>(fields[j]);
          if(!Rcpp::NumericVector::is_na(embeddings(i, j))){
            termfeatures.push_back(CRFSuite::Attribute(field, embeddings(i, j)));  
          }
        }
      }
      termsequenceattributes.push_back(termfeatures);
    }
    
    // Do the prediction & add things to the general output
    model.set(termsequenceattributes);
    std::vector<std::string> labels_group = model.viterbi();
    for(unsigned int scoreidx=0; scoreidx<labels_group.size(); scoreidx++) { 
      labels.push_back(labels_group[scoreidx]);
      marginal.push_back(model.marginal(labels_group[scoreidx], scoreidx)); 	
    }
    sequences.push_back(previous_group);
    sequence_probabilities.push_back(model.probability(labels_group));
    
    // The start point of the next group is the current row
    group_start = rowidx;
    previous_group = current_group;
  }
  
  // Return viterbi output + sequence probabilities
  Rcpp::List out = Rcpp::List::create(
    Rcpp::Named("viterbi") = Rcpp::DataFrame::create(
      Rcpp::Named("label") = labels,
      Rcpp::Named("marginal") = marginal,
      Rcpp::Named("stringsAsFactors") = false), 
    Rcpp::Named("sequence") = Rcpp::DataFrame::create(
      Rcpp::Named("group") = sequences,
      Rcpp::Named("probability") = sequence_probabilities,
      Rcpp::Named("stringsAsFactors") = false));
  return out;
}





