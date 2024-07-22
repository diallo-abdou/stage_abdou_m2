
# Flags
FLAGS <- flags(flag_integer('dense_units1', 64),
               flag_integer('dense_units2', 32),
               flag_integer('dense_units3', 16),
               flag_integer('dense_units4', 8),
               flag_numeric('dropout1', 0.4),
               flag_numeric('dropout2', 0.3),
               flag_numeric('dropout3', 0.2),
               flag_numeric('dropout4', 0.1),
               flag_integer('batch_size', 16))



# Model
model <- keras_model_sequential() %>% 
  layer_dense(units = FLAGS$dense_units1, activation = 'relu',input_shape = c(25)) %>% 
  layer_dropout(rate = FLAGS$dropout1) %>% 
  
  layer_dense(units = FLAGS$dense_units2,activation = 'relu') %>% 
  layer_dropout(rate = FLAGS$dropout2) %>% 
  
  layer_dense(units = FLAGS$dense_units3,activation = 'relu') %>% 
  layer_dropout(rate = FLAGS$dropout3) %>% 
  
  layer_dense(units = FLAGS$dense_units4,activation = 'relu') %>% 
  layer_dropout(rate = FLAGS$dropout4) %>% 
  
  layer_dense(units = 1)



# Compile
model %>% compile(loss = 'mse',
                      optimizer = 'rmsprop',
                      metrics = 'mae')


#  callback EarlyStopping
mon_callback <- callback_early_stopping(
  monitor = "val_mae",  # Surveille la perte sur l'ensemble de validation
  patience = 10,         # Nombre d'époques sans amélioration avant l'arrêt
  restore_best_weights = TRUE  # Restaure les poids du meilleur modèle
)


# Fit
history <- model %>% 
  fit(training,
      trainingtarget,
      epochs = 50,
      batch_size = FLAGS$batch_size,
      validation_split = 0.2,
      callbacks = list(mon_callback))





