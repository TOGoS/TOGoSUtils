<?php

return array(
	'repositories' => array(
		'some-repository' => '/var/ccouch'
	),
	'allow-uploads' => false, // 'for-authorized-users' not yet implemented
	'upload-repository' => 'some-repository',
	'upload-sector' => 'user-content',
	'allow-upload-sector-override' => false, // Allow override via like a X-CCouch-Sector: header or something
	'http-response-headers' => array(
		// Any additional headers you want to send.
		// These will be combined with automatically generated ones.
		// e.g. if you specify
		//   access-control-allow-methods: DELETE,
		// the resulting header will be something like
		//   access-control-allow-methods: HEAD, GET, OPTIONS, DELETE
		'access-control-allow-origin' => '*',
	),
);
