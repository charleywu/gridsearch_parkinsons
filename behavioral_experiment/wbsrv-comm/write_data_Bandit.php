<?php
// this path should point to your configuration file.
include('../../config/server.config.php');
//$rest_json = file_get_contents("php://input");
//$_POST = json_decode($rest_json, true);

$data_array_raw = json_decode($_POST['data'], true);

$data_array = [];
$data_array["uid"] = $data_array_raw["uid"];
$data_array["data"] = json_encode($data_array_raw); 

$data_array = [$data_array];

//var_dump($data_array);
//exit();

//$data_array=json_decode($data_array);
//$data_array = json_decode($json);
#var_dump($data_array)
try {
  $conn = new PDO("mysql:host=$servername;port=$port;dbname=$dbname", $username, $password);
  $conn->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
  // First stage is to get all column names from the table and store
  // them in $col_names array.
  $stmt = $conn->prepare("SHOW COLUMNS FROM `$table_bandit`");
  $stmt->execute();
  $col_names = array();
  while($row = $stmt->fetchColumn()) {
    $col_names[] = $row;
  }
  
  //var_dump($col_names); exit;
  
  // Second stage is to create prepared SQL statement using the column
  // names as a guide to what values might be in the JSON.
  // If a value is missing from a particular trial, then NULL is inserted
  $sql = "INSERT INTO $table_bandit VALUES(";
  for($i = 0; $i < count($col_names); $i++){
    $name = $col_names[$i];
    $sql .= ":$name";
    if($i != count($col_names)-1){
      $sql .= ", ";
    }
  }
  $sql .= ");";
  $insertstmt = $conn->prepare($sql);
  for($i=0; $i < count($data_array); $i++){
    for($j = 0; $j < count($col_names); $j++){
      $colname = $col_names[$j];
      if(!isset($data_array[$i][$colname])){
        $insertstmt->bindValue(":$colname", null, PDO::PARAM_NULL);
      } else {
      	$val = $data_array[$i][$colname];
      	
      	if( is_array($val) ){
      		$val = implode(",", $val);
      	}

        $insertstmt->bindValue(":$colname", $val);
      }
    }
    $insertstmt->execute();
  }
  //echo file_get_contents('php://input');
  header('HTTP/1.1 200 Success', true, 200);
  return 'Success';
} catch(PDOException $e) {
    echo '{"success": false, "message": ' . $e->getMessage();
}
$conn = null;
?>